#!/bin/bash

# OS settings
DATAVIRT_REG_IMG='registry.access.redhat.com/jboss-datavirt-6/datavirt63-openshift'
DATAVIRT_IMG='jboss-datavirt63-openshift'
OS_TEMPLATE='komodo-teiid-wildfly-s2i'

OPENSHIFT_PROJECT=vdb-builder
OPENSHIFT_APPLICATION_NAME=vdb-builder-openshift
OPENSHIFT_SERVICE_ACCOUNT=vdb-builder-service-account
OPENSHIFT_APP_SECRET=vdb-builder-app-secret

# source repository
SOURCE_REPOSITORY_URL=https://github.com/phantomjinx/teiid-komodo
SOURCE_REPOSITORY_REF=master

# https keystore
DSB_SERVER_KEYSTORE_DIR=security
DSB_SERVER_KEYSTORE_DEFAULT=server.keystore
DSB_SERVER_KEYSTORE_DEFAULT_ALIAS=jboss
DSB_SERVER_KEYSTORE_DEFAULT_PASSWORD=raleigh

# Teiid credentials
TEIID_USERNAME=user
TEIID_PASSWORD=user1234!
