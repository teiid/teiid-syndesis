/*
 * JBoss, Home of Professional Open Source.
 * See the COPYRIGHT.txt file distributed with this work for information
 * regarding copyright ownership.  Some portions may be licensed
 * to Red Hat, Inc. under one or more contributor license agreements.
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2.1 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
 * 02110-1301 USA.
 */
package org.komodo.servicecatalog;

import java.util.ArrayList;
import java.util.Collection;
import java.util.List;
import java.util.Map;
import java.util.TreeMap;
import org.komodo.relational.vdb.Vdb;
import org.komodo.spi.constants.StringConstants;
import org.komodo.spi.repository.ApplicationProperties;
import org.komodo.utils.KLog;
import io.fabric8.kubernetes.api.model.EnvVar;
import io.fabric8.kubernetes.api.model.ObjectReference;
import io.fabric8.kubernetes.api.model.ObjectReferenceBuilder;
import io.fabric8.openshift.api.model.ImageStream;
import io.fabric8.openshift.api.model.TagReference;
import io.fabric8.openshift.client.OpenShiftClient;

public class PublishConfiguration implements StringConstants {

    private static final String DEFAULT_NAMESPACE = "openshift";

    private static final String BASE_OPENSHIFT_IMAGE = "redhat-openjdk18-openshift";

    protected Vdb vdb;
    protected boolean enableOdata = true;
    protected String containerMemorySize = "1024Mi";
    protected long buildTimeoutInSeconds = 2 * 60 * 1000L;
    protected List<EnvVar> allEnvironmentVariables = new ArrayList<>();
    
    // cpu units
    private int cpuUnits = 500; //100m is 0.1 of CPU, at 500m we have 1/2 CPU as default
    
    public void setVDB(Vdb vdb) {
        this.vdb = vdb;
    }
    
    public void setEnableOData(boolean flag) {
        this.enableOdata = flag;
    }
    
    public void setContainerMemorySize(String size) {
        this.containerMemorySize = size;
    }
    
    public void addEnvironmentVariables(Collection<EnvVar> envs) {
        if (envs != null && !envs.isEmpty()) {
            this.allEnvironmentVariables.addAll(envs);
        }
    }     
    
    protected ObjectReference getBaseJDKImage(OpenShiftClient client) {
        //
        // Search the openshift namespace by default but if the base image cannot be
        // found then try the application namespace just to be sure
        //
        String[] namespaces = new String[] { DEFAULT_NAMESPACE, ApplicationProperties.getNamespace() };
        ImageStream is = null;
        for (String namespace : namespaces) {
            is = client.imageStreams().inNamespace(namespace).withName(BASE_OPENSHIFT_IMAGE).get();
            if (is != null) {
                List<TagReference> tagRef = is.getSpec().getTags();
                String tag = tagRef.get(tagRef.size()-1).getName();
                KLog.getLogger().debug("Using " + BASE_OPENSHIFT_IMAGE + COLON + tag + " as the base image for the vdb based service");
                return new ObjectReferenceBuilder().withKind("ImageStreamTag").withNamespace(namespace)
                        .withName(BASE_OPENSHIFT_IMAGE + COLON + tag).build();

                /*
                 * It has been observed that swarm application is not able start successfully with this image. 
                 * More investigation is needed.
                KLog.getLogger().debug("Using fabric8/s2i-java:latest as the base image for the vdb based service");
                return new ObjectReferenceBuilder().withKind("DockerImage")
                        .withName("fabric8/s2i-java:latest").build();
                */
            }
        }

        return null;
    }
    
    protected String getUserJavaOptions() {
        StringBuilder sb = new StringBuilder();
        sb.append(" -XX:+UnlockExperimentalVMOptions");
        sb.append(" -XX:+UseCGroupMemoryLimitForHeap");
        sb.append(" -Djava.net.preferIPv4Addresses=true");
        sb.append(" -Djava.net.preferIPv4Stack=true");

        // CPU specific JVM options
        sb.append(" -XX:ParallelGCThreads="+cpuLimit());
        sb.append(" -XX:ConcGCThreads="+cpuLimit());
        sb.append(" -Djava.util.concurrent.ForkJoinPool.common.parallelism="+cpuLimit());
        sb.append(" -Dio.netty.eventLoopThreads="+(2*cpuLimit()));
        return sb.toString();
    }
    
    
    protected Map<String, String> getUserEnvironmentVariables() {
        Map<String, String> envs = new TreeMap<>();
        envs.put("AB_JOLOKIA_OFF", "true");
        envs.put("AB_OFF", "true");
        envs.put("GC_MAX_METASPACE_SIZE", "256");
        return envs;
    }    
    
    protected String cpuUnits() {
        return Integer.toString(cpuUnits)+"m";
    }
    
    private int cpuLimit() {
        return Math.max(cpuUnits/1000, 1);
    }    
}
