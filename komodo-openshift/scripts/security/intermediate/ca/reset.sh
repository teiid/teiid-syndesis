#!/bin/bash

INTERMEDIATE_KEY="intermediate.key.pem"
INTERMEDIATE_CSR="intermediate.csr.pem"
INTERMEDIATE_CERT="intermediate.cert.pem"
CHAIN_CERT="ca-chain.cert.pem"

#
# Clean away any generated keys
#
for f in `ls private`
do
  # Keep intermediate key
  if [ "${f}" == "${INTERMEDIATE_KEY}" ]; then
    continue
  fi

  echo "Removing private key ${f}"
  rm -f private/${f}
done

#
# Clean away any generated CSRs
for f in `ls csr`
do
  # Keep intermediate csr
  if [ "${f}" == "${INTERMEDIATE_CSR}" ]; then
    continue
  fi

  echo "Removing csr ${f}"
  rm -f csr/${f}
done

#
# Clean away any generated certificates
#
for f in `ls certs`
do
  # Keep intermediate certificate
  if [ "${f}" == "${INTERMEDIATE_CERT}" ]; then
    continue
  fi

  # Keep intermediate/root chain certificate
  if [ "${f}" == "${CHAIN_CERT}" ]; then
    continue
  fi

  echo "Removing certificate ${f}"
  rm -f certs/${f}
done

rm -f newcerts/*.pem

if [ -f serial.old ]; then
  mv serial.old serial
fi

rm index.txt*
touch index.txt

rm -rf pkcs12
rm -rf jks
rm -f ../../server.keystore
