teiid-komodo
============

## Building teiid-komodo locally
- Install JDK 1.8 or higher;
- Install [maven 3.2+](http://maven.apache.org/download.html);
- By default, building will try to construct a docker image so a running Docker daemon is required. To skip this use the 'skip-docker' switch of the build script;
- Clone the repository and execute the '**build.sh**' script.

### Optional switches to build script
* -d: enable maven debugging (adds -e -X -U);
* -h: show the script help;
* -s: skip unit test execution;
* -q: enable integration test execution;
* -z: skip docker image creation and deployment.

## Running teiid-komodo on [Openshift](https://www.openshift.org)

There are two methods available for running teiid-komodo.

### Using the locally-built image
Build teiid-komodo locally then once built, the docker image and Dockerfile will be available in 'server/komodo-rest/target/docker'. It should also have been registered in the local docker registry.

An Openshift template that utilises this image is available [here](https://github.com/rareddy/osb-data-services/blob/master/komodo/data-access-service.json). This should be download and loaded into Openshift, inc. [minishift](https://github.com/minishift/minishift).



### Building inside Openshift
An alternative template is available at [komodo-openshift/scripts/komodo-teiid-wildfly-swarm-s2i.json](https://github.com/teiid/teiid-komodo/blob/master/komodo-openshift/scripts/komodo-teiid-wildfly-swarm-s2i.json). This template makes use of Openshift's s2i feature to build and then deploy teiid-komodo.

A unix command-line [script](https://github.com/teiid/teiid-komodo/blob/master/komodo-openshift/scripts/komodo-os-setup.sh) is provided to ingest the template and its dependencies into a default project named 'vdb-builder'. The script requires an url (using the -h switch) that points to the Openshift instance (whether Openshift, minishift), eg.

> ./komodo-os-setup.sh -h 192.168.88.5

The result of instantiating the template will be the following pods:

* wildfly-swarm-build-1-build - teiid-komodo is implemented on wildfly swarm hence requires this dependency build;
* vdb-builder-build-1-build - an s2i build of teiid-komodo;
* vdb-builder-1-xxxxx - a fully running implementation of teiid-komodo, a.k.a. vdb-builder (will only appear if the build completed);
* vdb-builder-gateway-xxxxxxxx-xxxx - the external interface allowing access to teiid-komodo via https (port 8443);
* vdb-builder-persistence-1-xxxxx - the database instance that provides the persistence store for teiid-komodo;

Routes are provided for accessing vdb-builder's external interfaces.

* https://vdb-builder-gateway-vdb-builder.xxx.xxx.xxx.xxx.nip.io/ provides a default landing page;
* https://vdb-builder-gateway-vdb-builder.xxx.xxx.xxx.xxx.nip.io/vdb-builder/api-docs/ provides a [Swagger](https://swagger.io/) document of teiid-komodo's REST API;
* https://vdb-builder-gateway-vdb-builder.xxx.xxx.xxx.xxx.nip.io/vdb-builder/v1/swagger.json provides a json document of the teiid-komodo REST API.

#### Additional Options
The *komodo-os-setup.sh* provides some additional options for building and deployment of teiid-komodo:
* -m url: To improve building performance it is possible to specify a maven mirror url, which will be used for fetching all maven dependencies from. Installation of nexus on a local network host can facilitate far faster build-times as it effectively caches all the dependencies;
-s url: The source repository, by default, is (https://github.com/teiid/teiid-komodo). Should an alternative be required, eg. a fork to test new functionality, then this can be specified and new builds will use that instead;
-r local mvn repo: An additional maven repository can be specified that can be checked for maven dependencies. This is useful, for example, if custom / snapshot builds of dependencies are required that have not yet been pushed to a public maven repository.

#### Troubleshooting

##### DNS Resolution in Minishift
A Minishift VM creates its own network adapter for connecting to the host OS which in turn allows full network connectivity. Resolving of DNS hostnames is performed using the same system. Sometimes it is possible for the Minishift instance to be started without full DNS resolution being available. In this case, hostnames, such as 'archive.apache.org', fail to resolve causing the s2i build of teiid-komodo to fail. Restarting the minishift instance can rectify this.

##### Hostname 
In Minishift, the VM is unable to see hostnames documented in the host OS' /etc/hosts file yet it is possible to resolve IP addresses. Only if the hostnames are inserted in the VM's own /etc/hosts will they be resolved correctly.

##### Openshift Storage Space
Openshift and Docker are responsible for generating a significant amount of content which can fill up HDD space, including docker images and persistent volumes. If space becomes too low then errors can start to appear. Cleaning up docker images in Minishift can require secure-shell into the VM then manually deleting the images using the native docker commands. Likewise, persistent volumes can require manually deletion of their directories.

##### Memory and Storage Capacity Variables
By default, Minishift starts with 2GB of memory and 20GB of storage. This can quickly prove insufficient for developing teiid-komodo. It is advisable to increase these values, eg.

> minishift config set disk-size 60GB

> minishift config set memory 6GB
