#!/bin/bash

CA_PASSWORD="raleigh"
DEFAULT_ALIAS="jboss"
DEFAULT_KEYSTORE="server.keystore"

#
# Show help and exit
#
function show_help {
        echo "Usage: $0 -d <domain> [-p] <password> [-h]"
        echo "-d - target domain of certificate"
        echo "-p - password to be used for private key and key stores (${CA_PASSWORD} by default)"
        echo "-a - alias to certificate in keystore (${DEFAULT_ALIAS} by default)"
        echo "-k - name of the keystore (${DEFAULT_KEYSTORE} by default)"
        echo "-h - show help"
  exit 1
}

#
# Check there is an openssl.cnf file
#
if [ ! -f "openssl.cnf" ]; then
  echo "No openssl configuration file ... exiting"
  exit 1
fi

#
# Check openssl and keytool are installed
#
type openssl >/dev/null 2>&1 || { echo >&2 "openssl is required but not installed ... exiting"; exit 1; }
type keytool >/dev/null 2>&1 || { echo >&2 "keytool is required but not installed ... exiting"; exit 1; }

#
# Determine the command line options
#
# Very first ':' switches to silent mode
# Colons following letters => argument required
# 
while getopts ":d:p:a:k:h" opt;
do
  case $opt in
    d) DOMAIN=$OPTARG ;;
    p) PASSWORD=$OPTARG ;;
    a) ALIAS=$OPTARG ;;
    k) KEYSTORE=$OPTARG ;;
    h) show_help ;;
    *) show_help ;;
  esac
done

shift `expr $OPTIND - 1`

#
# Check we have a domain
#
if [ -z "${DOMAIN}" ]; then
  echo "No domain specified. Execute as $0 <domain> ... exiting"
  exit 1
fi

#
# Check the password
#
if [ -z "${PASSWORD}" ]; then
  PASSWORD="${CA_PASSWORD}"
  echo "=== Key password left as default: ${CA_PASSWORD} ==="
fi

#
# Check the alias
#
if [ -z "${ALIAS}" ]; then
  ALIAS=${DEFAULT_ALIAS}
  echo "=== Keystore alias left as default: ${ALIAS} ==="
fi

#
# Check the keystore name
#
if [ -z "${KEYSTORE}" ]; then
  KEYSTORE=${DEFAULT_KEYSTORE}
  echo "=== Keystore filename left as default: ${KEYSTORE} ==="
fi

echo "=== Generating certificate (including its key, csr and p12 keystore) and importing it into a keystore ==="
echo "=== DOMAIN: ${DOMAIN} ==="
echo "=== PASSWORD: ${PASSWORD} (used for key, p12 keystore and keystore) ==="
echo "=== KEYSTORE NAME: ${KEYSTORE} ==="
echo "=== KEYSTORE ALIAS: ${ALIAS} ==="

#
# Create directories if not already created
#
mkdir -p newcerts private csr certs

#
# Create the domain key
#
if [ ! -f private/${DOMAIN}.key.pem ]; then
  echo "=== Generating private key for domain ==="
  openssl genrsa -aes256 \
    -passout pass:${PASSWORD} \
    -out private/${DOMAIN}.key.pem 2048
else
  echo "=== Skipping private key generation for domain ... already exists ==="
fi

chmod 400 private/${DOMAIN}.key.pem

if [ ! -f "private/${DOMAIN}.key.pem" ]; then
  echo "Key failed to generate ... exiting"
  exit 1
fi

#
# Create certificate request for the domain
#

COUNTRY="US"
STATE="North Carolina"
LOCALITY="Raleigh"
ORGANISATION="Redhat"
OU="Testing"
COMMONNAME=${DOMAIN}
EMAIL="teiid-designer@lists.jboss.org"

echo "=== Generating CSR from key ==="
openssl req \
  -config openssl.cnf \
  -key private/${DOMAIN}.key.pem \
  -new -sha256 \
  -out csr/${DOMAIN}.csr.pem \
  -passin pass:${PASSWORD} \
  -subj "/C=${COUNTRY}/ST=${STATE}/L=${LOCALITY}/O=${ORGANISATION}/OU=${OU}/CN=${COMMONNAME}/emailAddress=${EMAIL}"

if [ ! -f "csr/${DOMAIN}.csr.pem" ]; then
  echo "CSR failed to generate ... exiting"
  exit 1
fi

#
# Generate the certificate by signing the CSR with the intermediate key
#
echo "=== Generating signed certificate from CSR ==="
openssl ca \
  -config openssl.cnf \
  -passin pass:${CA_PASSWORD} \
  -batch \
  -extensions server_cert -days 375 -notext -md sha256 \
  -in csr/${DOMAIN}.csr.pem \
  -out certs/${DOMAIN}.cert.pem

if [ ! -f "certs/${DOMAIN}.cert.pem" ]; then
  echo "Certificate failed to generate ... exiting"
  exit 1
fi

chmod 444 certs/${DOMAIN}.cert.pem

#
# Verify the certificate
#
echo "=== Verifying certificate ==="
openssl x509 -noout -text \
  -in certs/${DOMAIN}.cert.pem

#
# Convert the certificate into a PKCS12 bundle
#
echo "=== Generating p12 keystore in pkcs12 directory ==="
mkdir -p pkcs12
openssl pkcs12 \
  -export \
  -in certs/${DOMAIN}.cert.pem \
  -inkey private/${DOMAIN}.key.pem \
  -passin pass:${PASSWORD} \
  -chain \
  -CAfile certs/ca-chain.cert.pem \
  -name "${DOMAIN}" \
  -password pass:${PASSWORD} \
  -out pkcs12/${DOMAIN}.cert.p12

if [ ! -f "pkcs12/${DOMAIN}.cert.p12" ]; then
  echo "p12 keystore failed to generate ... exiting"
  exit 1
fi

#
# Import into keystore
#
echo "=== Importing certificate into jks keystore at jks/${KEYSTORE} ==="

mkdir -p jks

if [ -f jks/${KEYSTORE} ]; then
  rm -f jks/${KEYSTORE}
fi

keytool -importkeystore \
  -srckeystore pkcs12/${DOMAIN}.cert.p12 \
  -srcstoretype PKCS12 \
  -srcstorepass ${PASSWORD} \
  -srcalias ${DOMAIN} \
  -deststorepass ${PASSWORD} \
  -destkeystore jks/${KEYSTORE} \
  -destalias ${ALIAS}

if [ ! -f "jks/${KEYSTORE}" ]; then
  echo "jks keystore failed to generate ... exiting"
  exit 1
fi

echo "=== keystore generated at jks/${KEYSTORE} ==="
keytool -list \
  -keystore jks/${KEYSTORE} \
  -storepass ${PASSWORD}

echo "=== Copying keystore to expected script location ==="
cp jks/${KEYSTORE} ../../server.keystore
