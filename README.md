teiid-syndesis
============
[![Build Status](https://travis-ci.org/teiid/teiid-syndesis.svg?branch=master)](https://travis-ci.org/teiid/teiid-syndesis)

Teiid tooling is now part of the [Syndesis](https://syndesis.io) project. This works a component inside Syndesis to provide data integration capabilities. This document is for developers who like to setup a workspace locally for testing or for development purposes.

## Prerquisites for Developers
- Install JDK 1.8 or higher
- Install [maven 3.2+](http://maven.apache.org/download.html)
- Install [Go Lang](https://developer.fedoraproject.org/tech/languages/go/go-installation.html) These instructions are for Linux, find similar for your platform.
- Install [minishift 3.9+](https://www.okd.io/minishift/); which is available for all the major operating systems (Linux, OS X and Windows). The following examples assume that you have Minishift installed and can be called with minishift from the command line. So, minishift is supposed to be available in your search path, i.e. located in a directory contained in your $PATH environment variable (Linux, macOS) or in a directory from your system path (Windows)
- Clone the Syndesis libraries.

```
git@github.com:syndesisio/syndesis.git
cd syndesis
```
- Clone Teiid Syndesis Repository 
```
git clone git@github.com:teiid/teiid-syndesis.git
```

- Now follow below script
```
cd syndesis
minishift addons enable admin-user

# install syndesis in minishift
tools/bin/syndesis minishift --install --project syndesis --full-reset --memory 8GB --disk-size 40GB --cpus 4 --datavirt --nodev

# wait until the Minishift is started and you logged using oc login, then proceed with next step

oc login -u system:admin
```
Note: If you are devloper and want your locally built images to be deployed to this minishift instance, then change `--nodev` in above command to `--dev`, however this will end up with few images in not started position. To fix that, you can do
```
cd teiid-syndesis
./fix4dev.sh
```

It will fix the images, and also when you update the image from local development, it will not be overridden anymore.

At this point Syndesis should be running with `syndesis-dv` in it, now if you are working on any changes for development in teiid-syndesis, make you code edits, once done execute below

```
cd teiid-syndesis
oc login -u developer (if new terminal)
oc project syndesis (if current project is myproject)
mvn install -Pimage [-DskipTests]
```

Once the above build is done, it will replace the existing `syndesis-dv` instance with lastest one that has been just built with your latest changes.

### Avoid updating with remote instance when working locally
When working in the development mode, the server image will automatically will be replaced with one in the Docker repo in 15 minutes, to avoid it run the following on your OpenShift after above installation.

```
oc edit syndesis app 
# then add below flag under "spec" section
spec:
  devSupport: true
```
save and exit. Now on your development machine when you do 

```
mvn install -Pimage [-DskipTests]
```

The image generated will be the new `syndesis-dv` image

#### Remote Debugging
To enable the debugging of "syndesis-dv" instance, go to the OpenShift console where the application is deployed, and in the `syndesis-dv` deployment config's Environment variables add `JAVA_DEBUG` to `true` and then recyle the current pod such that new pod will be generated with new environment property.

The built images are deployed with java virtual machine debugging enabled, using port 5005. This allows for developers to remotely connect to the running vdb-builder using an IDE like [Eclipse](https://www.eclipse.org).

The port 5005 is not offered by any of the routes, although a route can be setup if preferred. A quick alternative is to [port-forward](https://docs.openshift.com/enterprise/3.0/dev_guide/port_forwarding.html) the port to the localhost using the following command:

> oc get pods

> oc port-forward syndesis-dv-x-yyyy 5005:5005

where *vdb-builder-x-yyyy* is the latest deployed `syndesis-dv` pod.

If using Eclipse, open the _Debugging Configurations_ window and create a new _Remote Java Application_ configuration. Setting the host to **localhost** and the port to **5005**, the configuration can then be executed. The debug perspective will indicate the connection has been successfully undertaken and all breakpoints will be fired by the vdb-builder image.

#### Troubleshooting

##### Hostname 
In Minishift, the VM is unable to see hostnames documented in the host OS' /etc/hosts file yet it is possible to resolve IP addresses. Only if the hostnames are inserted in the VM's own /etc/hosts will they be resolved correctly.

##### Openshift Storage Space
Openshift and Docker are responsible for generating a significant amount of content which can fill up HDD space, including docker images and persistent volumes. If space becomes too low then errors can start to appear. Cleaning up docker images in Minishift can require secure-shell into the VM then manually deleting the images using the native docker commands. Likewise, persistent volumes can require manually deletion of their directories.

##### Memory and Storage Capacity Variables
By default, Minishift starts with 2GB of memory and 20GB of storage. This can quickly prove insufficient for developing syndesis-dv. It is advisable to increase these values, eg.

> minishift config set disk-size 60GB

> minishift config set memory 8GB

See also the optional parameters you can supply on the "syndesis minishift" command to achieve the same.


Licenses
-------

The default license is the [Apache Software License (ASL) v2.0][1]

Where applicable individual modules or javascript libraries will provide other copyright and license information.

[1]: view-source:https://www.apache.org/licenses/LICENSE-2.0
=======
