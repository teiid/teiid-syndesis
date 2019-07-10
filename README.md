teiid-komodo
============
[![Build Status](https://travis-ci.org/teiid/teiid-komodo.svg?branch=master)](https://travis-ci.org/teiid/teiid-komodo)

Teiid Komodo is now part of the [Syndesis](https://syndesis.io) project. This works a component inside Syndesis to provide data integration capabilities. This document is for developers who like to setup a workspace locally for testing or for development purposes.

## Prerquisites for Developers
- Install JDK 1.8 or higher
- Install [maven 3.2+](http://maven.apache.org/download.html)
- Install [Go Lang](https://developer.fedoraproject.org/tech/languages/go/go-installation.html) These instructions are for Linux, find similar for your platform.
- Install [minishift 3.9+](https://www.okd.io/minishift/); which is available for all the major operating systems (Linux, OS X and Windows). The following examples assume that you have Minishift installed and can be called with minishift from the command line. So, minishift is supposed to be available in your search path, i.e. located in a directory contained in your $PATH environment variable (Linux, macOS) or in a directory from your system path (Windows)
- Clone the Syndesis libraries. (Note this is Ramesh's branch, until code is merged into Syndesis repo, we would need to use this. Once the code is merged we can use Syndesis repo directly
```
git clone --branch=teiid-syndesis git@github.com:rareddy/syndesis.git
cd syndesis
```
- Clone Teiid Komodo Repository 
```
git clone git@github.com:teiid/teiid-komodo.git
```

- Now follow below script
```
cd syndesis
minishift addons enable admin-user

# install syndesis in minishift
tools/bin/syndesis minishift --install --project syndesis --vm-driver virtualbox --full-reset --memory 8GB --disk-size 40GB

# wait until the Minishift is started and you logged using oc login, then proceed with next step

oc login -u system:admin

tools/bin/syndesis install --setup --grant developer --cluster --project syndesis

# Build syndesis templates with Komodo images
install/generator/run.sh

# build the Syndesis operator project to pick up the new template with Komodo Image
tools/bin/syndesis build -m operator -i

# invoke the operator with new template, replace will replace previous duplicate ones
oc replace -f install/operator/deploy/syndesis.yml

```

At this point Syndesis should be running with `komodo-server` in it, now if you are working on any changes for development in komodo, make you code edits, once done execute below
```
cd teiid-komodo
oc login -u developer (if new terminal)
oc project syndesis (if current project is myproject)
mvn install -Pimage [-DskipTests]
```

Once the above build is done, it will replace the existing `komodo-server` instance with lastest one that has been just built with your latest changes.

### Avoid updating with remote instance when working locally
When working in the development mode, the server image will automatically will be replaced with one in the Docker repo in 15 minutes, to avoid it run the following on your OpenShift after above installation.

```
oc patch is komodo-server --type json -p="[{\"op\": \"replace\", \"path\": \"/spec/tags/0/from/kind\", \"value\": \"ImageStreamTag\"},{\"op\": \"replace\", \"path\": \"/spec/tags/0/from/name\", \"value\": \"komodo-server:latest\"},{\"op\": \"remove\", \"path\": \"/spec/tags/0/importPolicy\"}]"
```

#### Remote Debugging
To enable the debugging of "komodo-server" instance, go to the OpenShift console where the application is deployed, and in the `komodo-server` deployment config's Environment variables add `JAVA_DEBUG` to `true` and then recyle the current pod such that new pod will be generated with new environment property.

The built images are deployed with java virtual machine debugging enabled, using port 5005. This allows for developers to remotely connect to the running vdb-builder using an IDE like [Eclipse](https://www.eclipse.org).

The port 5005 is not offered by any of the routes, although a route can be setup if preferred. A quick alternative is to [port-forward](https://docs.openshift.com/enterprise/3.0/dev_guide/port_forwarding.html) the port to the localhost using the following command:

> oc get pods

> oc port-forward komodo-server-x-yyyy 5005:5005

where *vdb-builder-x-yyyy* is the latest deployed `komodo-server` pod.

If using Eclipse, open the _Debugging Configurations_ window and create a new _Remote Java Application_ configuration. Setting the host to **localhost** and the port to **5005**, the configuration can then be executed. The debug perspective will indicate the connection has been successfully undertaken and all breakpoints will be fired by the vdb-builder image.

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

See also the optional parameters you can supply on the "syndesis minishift" command to achieve the same.


Licenses
-------

The default license is the [Apache Software License (ASL) v2.0][1]

Where applicable individual modules or javascript libraries will provide other copyright and license information.

[1]: view-source:https://www.apache.org/licenses/LICENSE-2.0
=======
