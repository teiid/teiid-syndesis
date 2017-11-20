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

echo -e '\n\n=== Logging into oc tool as admin ==='
oc login https://${OS_HOST}:8443 -u admin -p admin
oc whoami 2>&1 > /dev/null || { echo "Cannot log in ... exiting" && exit 1; }

echo "Switch to the new project, creating it if necessary"
{ oc get project ${OPENSHIFT_PROJECT} 2>&1 >/dev/null && \
	oc project ${OPENSHIFT_PROJECT}; } || \
	oc new-project ${OPENSHIFT_PROJECT} || \
	{ echo "FAILED: Could not use indicated project ${OPENSHIFT_PROJECT}" && exit 1; }

echo -e '\n\n=== Creating the template. ==='
oc get template ${OS_TEMPLATE} 2>&1 > /dev/null || \
	oc create -f ${OS_TEMPLATE}.json || \
	{ echo "FAILED: Could not create application template" && exit 1; }

echo -e "\n\n=== Deploying ${OS_TEMPLATE} template with default values ==="
oc get dc/vdb-builder 2>&1 >/dev/null || \
	oc new-app ${OS_TEMPLATE} \
		--param=TEIID_USERNAME=${TEIID_USERNAME} \
		--param=TEIID_PASSWORD=${TEIID_PASSWORD} \
		-l app=vdb-builder

echo "==============================================="
echo -e "\n\n=== Start the following builds if not already started:"
echo -e "\n\n=== 	1. oc start-build vdb-builder"
echo "==============================================="

echo "Done."