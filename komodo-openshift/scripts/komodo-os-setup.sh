#!/bin/bash

###
#
# Installs and configures a vdb-builder-teiid-wildfly instance
#
# Based on scripts from:
#
# * https://github.com/michaelepley/openshift-demo-jdv
# * https://github.com/cvanball/jdv-ose-demo
#
###

#################
#
# Show help and exit
#
#################
function show_help {
	echo "Usage: $0 -h"
	echo "-h - ip|hostname of Openshift host"
  exit 1
}

if [ ! -f 'config.sh' ]; then
    echo "No config file found .. exiting"
    exit 1
fi

#
# Source the configuration
#
. ./config.sh

#
# Determine the command line options
#
while getopts "h:" opt;
do
	case $opt in
	h) OS_HOST=$OPTARG ;;
	*) show_help ;;
	esac
done

if [ -z "$OS_HOST" ]; then
  echo "No Openshift host specified. Use -h <host|ip>"
  exit 1
fi

#
# Check that we have a server keystore for https
#
if [ ! -f ${DSB_SERVER_KEYSTORE_DIR}/${DSB_SERVER_KEYSTORE_DEFAULT} ]; then
    echo -e '\n\n === HTTPS keystore has not been generated. ==='
    echo -e '\tNavigate to security/intermediate/ca.'
    echo -e '\tExecute ./create-certificate.sh -d <domain>'
    echo -e '\t\twhere domain is the name of the https route.'
    echo -e '\t\t\tThis is in the format "secure-dsb-openshift-dsb.rhel-cdk.x.x.x.x.xip.io"'
    echo -e '\t\t\twhere x.x.x.x is the ip address of the openshift instance, eg. 10.1.1.2'
    exit 1
fi

echo -e '\n\n=== Logging into oc tool as admin ==='
oc login https://${OS_HOST}:8443 -u admin -p admin
oc whoami 2>&1 > /dev/null || { echo "Cannot log in ... exiting" && exit 1; }

echo "Switch to the new project, creating it if necessary"
{ oc get project ${OPENSHIFT_PROJECT} 2>&1 >/dev/null && \
	oc project ${OPENSHIFT_PROJECT}; } || \
	oc new-project ${OPENSHIFT_PROJECT} || \
	{ echo "FAILED: Could not use indicated project ${OPENSHIFT_PROJECT}" && exit 1; }

echo -e '\n\n=== Creating the template. This will live in the openshift namespace and be available to all projects ==='
oc get template ${OS_TEMPLATE} 2>&1 > /dev/null || \
	oc create -f ${OS_TEMPLATE}.json || \
	{ echo "FAILED: Could not create application template" && exit 1; }

echo -e '\n\n=== Creating a service account and accompanying secret for use by the dsb application ==='
oc get serviceaccounts ${OPENSHIFT_SERVICE_ACCOUNT} 2>&1 > /dev/null || \
	echo '{"kind": "ServiceAccount", "apiVersion": "v1", "metadata": {"name": "'${OPENSHIFT_SERVICE_ACCOUNT}'"}}' | oc create -f - || \
	{ echo "FAILED: could not create dsb service account" && exit 1; }

echo -e '\n\n=== Creating secrets for the DSB server ==='
oc get secret ${OPENSHIFT_APP_SECRET} 2>&1 > /dev/null || \
	oc secrets new ${OPENSHIFT_APP_SECRET} ${DSB_SERVER_KEYSTORE_DIR}/${DSB_SERVER_KEYSTORE_DEFAULT}

oc get sa/${OPENSHIFT_SERVICE_ACCOUNT} -o json | grep ${OPENSHIFT_APP_SECRET} 2>&1 > /dev/null || \
	oc secrets link ${OPENSHIFT_SERVICE_ACCOUNT} ${OPENSHIFT_APP_SECRET} || \
	{ echo "FAILED: could not link secret to service account" && exit 1; }

echo -e '\n\n=== Deploying ${OS_TEMPLATE} template with default values ==='
oc get dc/${OPENSHIFT_APPLICATION_NAME} 2>&1 >/dev/null || \
	oc new-app ${OS_TEMPLATE} \
		--param=SOURCE_REPOSITORY_URL=${SOURCE_REPOSITORY_URL} \
		--param=SOURCE_REPOSITORY_REF=${SOURCE_REPOSITORY_REF} \
		--param=SERVICE_ACCOUNT_NAME=${OPENSHIFT_SERVICE_ACCOUNT} \
		--param=HTTPS_SECRET=${OPENSHIFT_APP_SECRET} \
		--param=HTTPS_KEYSTORE=${DSB_SERVER_KEYSTORE_DEFAULT} \
		--param=HTTPS_NAME=${DSB_SERVER_KEYSTORE_DEFAULT_ALIAS} \
		--param=HTTPS_PASSWORD=${DSB_SERVER_KEYSTORE_DEFAULT_PASSWORD} \
		--param=TEIID_USERNAME=${TEIID_USERNAME} \
		--param=TEIID_PASSWORD=${TEIID_PASSWORD} \
		-l app=${OPENSHIFT_APPLICATION_NAME}

echo "==============================================="
echo -e "\n\n=== Start the following builds:"
echo -e "\n\n=== 	1. oc start-build ${OPENSHIFT_APPLICATION_NAME}"
echo "==============================================="

echo "Done."
