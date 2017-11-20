#!/bin/bash

# OS settings
OS_TEMPLATE=komodo-teiid-wildfly-swarm-s2i

OPENSHIFT_PROJECT=vdb-builder
OPENSHIFT_SERVICE_ACCOUNT=vdb-builder-service-account
OPENSHIFT_APP_SECRET=vdb-builder-app-secret

# source repository
SOURCE_REPOSITORY_URL=https://github.com/phantomjinx/teiid-komodo
SOURCE_REPOSITORY_REF=master

# https keystore
VDB_KEYSTORE_DIR=security
VDB_KEYSTORE_DEFAULT=server.keystore
VDB_KEYSTORE_DEFAULT_ALIAS=jboss
VDB_KEYSTORE_DEFAULT_PASSWORD=raleigh

# Teiid credentials
TEIID_USERNAME=user
TEIID_PASSWORD=user1234!
