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

import java.io.IOException;
import java.io.InputStream;
import java.util.ArrayList;
import java.util.Base64;
import java.util.Collection;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Properties;
import java.util.Set;
import java.util.TreeMap;
import java.util.concurrent.ConcurrentLinkedQueue;
import java.util.concurrent.CountDownLatch;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.TimeUnit;
import org.jboss.shrinkwrap.api.GenericArchive;
import org.jboss.shrinkwrap.api.ShrinkWrap;
import org.jboss.shrinkwrap.api.asset.ByteArrayAsset;
import org.jboss.shrinkwrap.api.asset.StringAsset;
import org.jboss.shrinkwrap.api.exporter.TarExporter;
import org.komodo.relational.model.Model;
import org.komodo.relational.vdb.ModelSource;
import org.komodo.relational.vdb.Vdb;
import org.komodo.rest.AuthHandlingFilter.AuthToken;
import org.komodo.rest.TeiidSwarmMetadataInstance;
import org.komodo.servicecatalog.BuildStatus.RouteStatus;
import org.komodo.servicecatalog.BuildStatus.Status;
import org.komodo.servicecatalog.datasources.AmazonS3Definition;
import org.komodo.servicecatalog.datasources.DefaultServiceCatalogDataSource;
import org.komodo.servicecatalog.datasources.ExcelDefinition;
import org.komodo.servicecatalog.datasources.FileDefinition;
import org.komodo.servicecatalog.datasources.MongoDBDefinition;
import org.komodo.servicecatalog.datasources.MySQLDefinition;
import org.komodo.servicecatalog.datasources.ODataV4Definition;
import org.komodo.servicecatalog.datasources.PostgreSQLDefinition;
import org.komodo.servicecatalog.datasources.SalesforceDefinition;
import org.komodo.servicecatalog.datasources.WebServiceDefinition;
import org.komodo.spi.KException;
import org.komodo.spi.constants.StringConstants;
import org.komodo.spi.logging.KLogger;
import org.komodo.spi.repository.ApplicationProperties;
import org.komodo.spi.repository.Repository.UnitOfWork;
import org.komodo.spi.runtime.ServiceCatalogDataSource;
import org.komodo.utils.KLog;
import org.teiid.adminapi.AdminException;
import org.teiid.core.util.ObjectConverterUtil;
import com.fasterxml.jackson.databind.ObjectMapper;
import io.fabric8.kubernetes.api.KubernetesHelper;
import io.fabric8.kubernetes.api.builds.Builds;
import io.fabric8.kubernetes.api.model.ContainerPort;
import io.fabric8.kubernetes.api.model.EnvVar;
import io.fabric8.kubernetes.api.model.EnvVarBuilder;
import io.fabric8.kubernetes.api.model.ObjectMeta;
import io.fabric8.kubernetes.api.model.ObjectReference;
import io.fabric8.kubernetes.api.model.Pod;
import io.fabric8.kubernetes.api.model.Quantity;
import io.fabric8.kubernetes.api.model.Service;
import io.fabric8.kubernetes.client.Config;
import io.fabric8.kubernetes.client.ConfigBuilder;
import io.fabric8.kubernetes.client.DefaultKubernetesClient;
import io.fabric8.kubernetes.client.KubernetesClient;
import io.fabric8.kubernetes.client.KubernetesClientException;
import io.fabric8.kubernetes.client.Watch;
import io.fabric8.kubernetes.client.Watcher;
import io.fabric8.openshift.api.model.Build;
import io.fabric8.openshift.api.model.BuildConfig;
import io.fabric8.openshift.api.model.BuildList;
import io.fabric8.openshift.api.model.DeploymentCondition;
import io.fabric8.openshift.api.model.DeploymentConfig;
import io.fabric8.openshift.api.model.ImageStream;
import io.fabric8.openshift.api.model.Route;
import io.fabric8.openshift.api.model.RouteList;
import io.fabric8.openshift.api.model.RouteSpec;
import io.fabric8.openshift.client.OpenShiftClient;
import io.fabric8.openshift.client.OpenShiftConfig;
import io.kubernetes.client.LocalObjectReference;
import io.kubernetes.client.ModelServiceCatalogClient;
import io.kubernetes.client.ParametersFromSource;
import io.kubernetes.client.Secret;
import io.kubernetes.client.ServiceBinding;
import io.kubernetes.client.ServiceBindingList;
import io.kubernetes.client.ServiceInstance;
import io.kubernetes.client.ServiceInstanceList;

public class TeiidOpenShiftClient implements StringConstants {

    private static final KLogger logger = KLog.getLogger();

    private static final String DAS = "das";
    private static final String MANAGED_BY = "managed-by";
    private static final String OSURL = "https://openshift.default.svc";
    private static final String SC_VERSION = "v1beta1";
    
    private static String CONTENT_TYPE = " -H 'Content-Type: application/json' ";
    private static String MANAGEMENT_URL = "http://127.0.0.1:9990/management";

    private ConcurrentLinkedQueue<BuildStatus> workQueue = new ConcurrentLinkedQueue<>();
    private boolean running = false;

    private TeiidSwarmMetadataInstance metadata;
    private HashMap<String, DataSourceDefinition> sources = new HashMap<>();
    private ModelServiceCatalogClient scClient;


    public TeiidOpenShiftClient(TeiidSwarmMetadataInstance metadata) {
        this.metadata = metadata;
        this.scClient = new ModelServiceCatalogClient(ApplicationProperties.getProperty("OSURL", OSURL), SC_VERSION);

        // data source definitions
        add(new PostgreSQLDefinition());
        add(new MySQLDefinition());
        add(new MongoDBDefinition());
        add(new FileDefinition());
        add(new ExcelDefinition());
        add(new ODataV4Definition());
        add(new SalesforceDefinition());
        add(new WebServiceDefinition());
        add(new AmazonS3Definition());
    }

    private void debug(String message) {
        if (! logger.isDebugEnabled())
            return;

        logger.debug(message);
    }

    private void add(DataSourceDefinition def) {
        sources.put(def.getType(), def);
    }

    /**
     * Returns DataSourceDefinition based on property sniffing
     * @param properties properties from service creation
     * @return DataSourceDefinition
     */
    private DataSourceDefinition getSourceDefinitionThatMatches(Map<String, String> properties) {
        for (DataSourceDefinition dd : this.sources.values()) {
            if (dd.isTypeOf(properties)) {
                return dd;
            }
        }
        return null;
    }

    /**
     * Returns DataSourceDefinition based on property sniffing
     * @param translatorName - Name of Translator
     * @return DataSourceDefinition
     */
    private DataSourceDefinition getSourceDefinitionThatMatchesTranslator(String translatorName) {
        for (DataSourceDefinition dsd : this.sources.values()) {
            if (dsd.getTranslatorName().equalsIgnoreCase(translatorName)) {
                return dsd;
            }
        }
        return null;
    }

    public Set<ServiceCatalogDataSource> getServiceCatalogSources(AuthToken authToken) throws KException {
        this.scClient.setAuthHeader(authToken.toString());
        Set<ServiceCatalogDataSource> sources = new HashSet<>();
        try {
            ServiceInstanceList serviceList = this.scClient.getServiceInstances(ApplicationProperties.getNamespace());
            if (serviceList != null) {
                List<ServiceInstance> services = serviceList.getItems();
                if( (services != null) && !services.isEmpty()) {
                    for (ServiceInstance svc : services) {
                        if (svc.getStatus().isReady()) {
                            DecodedSecret parameters = getParameters(svc);
                            if (parameters != null) {
                                DataSourceDefinition def = getSourceDefinitionThatMatches(parameters.getData());
                                if (def != null) {
                                    DefaultServiceCatalogDataSource scd = new DefaultServiceCatalogDataSource();
                                    scd.setName(svc.getMetadata().getName());
                                    scd.setTranslatorName(def.getTranslatorName());
                                    ServiceBinding binding = getServiceBinding(scd.getName());
                                    if (binding != null) {
                                        scd.setBound(true);
                                    }
                                    sources.add(scd);
                                    scd.setDefinition(def);
                                }
                            } else {
                                KLog.getLogger().info("Parameters not found for source "+ svc.getMetadata().getName());
                            }
                        }
                    }
                }
            }
        } catch (Exception e) {
            throw handleError(e);
        }
        return sources;
    }

    public void bindToServiceCatalogSource(AuthToken authToken, String dsName) throws KException {
        KLog.getLogger().info("Bind to Service:" + dsName);
        this.scClient.setAuthHeader(authToken.toString());
        try {
            ServiceInstance svc = this.scClient.getServiceInstance(ApplicationProperties.getNamespace(), dsName);
            if (svc == null) {
                throw new KException("No Service Catalog Service found with name " + dsName);
            }

            ServiceBinding binding = getServiceBinding(svc.getMetadata().getName());
            if (binding != null) {
                KLog.getLogger().debug("Found existing Binding = " + binding);
            }
            if (binding == null) {
                binding = this.scClient.createServiceBinding(svc);
                KLog.getLogger().debug("Created new Binding = " + binding);
            }

            //TODO: need to come up async based operation
            int i = 0;
            while(!binding.getStatus().isReady()) {
                Thread.sleep(5000);
                i++;
                binding = getServiceBinding(svc.getMetadata().getName());
                if ((i > 3) || (binding == null)) {
                    throw new KException("Created Service Binding is not Ready");
                }
            }
            
            DefaultServiceCatalogDataSource scd = buildServiceCatalogDataSource(svc, binding);
            Collection<String> dsNames = this.metadata.admin().getDataSourceNames();
            if (!dsNames.contains(dsName)) {
                createDataSource(dsName, scd);
            }
        } catch (Exception e) {
            throw handleError(e);
        }
    }
    
    private DecodedSecret getBindingSecrets(String secretName) throws IOException {
        Map<String, String> map = new TreeMap<>();
        Secret secret = this.scClient.getSecret(ApplicationProperties.getNamespace(), secretName);
        if (secret == null) {
            return null;
        }
        map = secret.getData();
        Map<String, String> decodedMap = new TreeMap<>();
        if ((map != null) && !map.isEmpty()) {
            for (Map.Entry<String, String> entry:map.entrySet()) {
                String key = entry.getKey();
                String encodedValue = entry.getValue();
                decodedMap.put(key, new String(Base64.getDecoder().decode(encodedValue)));
            }
        }
        map = decodedMap;
        return new DecodedSecret(secretName, map);
    }

    public DefaultServiceCatalogDataSource getServiceCatalogDataSource(AuthToken authToken, String dsName) throws KException {
        this.scClient.setAuthHeader(authToken.toString());
        try {
            ServiceInstance svc = this.scClient.getServiceInstance(ApplicationProperties.getNamespace(), dsName);
            if (svc == null) {
                return null;
            }
            ServiceBinding binding = getServiceBinding(svc.getMetadata().getName());
            if ((binding == null) || !binding.getStatus().isReady()) {
                return null;
            }
            return buildServiceCatalogDataSource(svc, binding);
        } catch (Exception e) {
            throw handleError(e);
        }
    }
    
    private DefaultServiceCatalogDataSource buildServiceCatalogDataSource(ServiceInstance svc, ServiceBinding binding)
            throws IOException {
        assert svc != null;
        assert binding != null;
        DecodedSecret parameters = getParameters(svc);
        DecodedSecret bindSecret = getBindingSecrets(binding.getSpec().getSecretName());
        Map<String, String> allProperties = new HashMap<>();
        if (parameters != null) {
            allProperties.putAll(parameters.getData());
        }
        if (bindSecret != null) {
            allProperties.putAll(bindSecret.getData());
        }
        DataSourceDefinition def = getSourceDefinitionThatMatches(allProperties);
        DefaultServiceCatalogDataSource scd = new DefaultServiceCatalogDataSource();
        scd.setName(svc.getMetadata().getName());
        scd.setTranslatorName(def.getTranslatorName());
        scd.setBound(true);
        scd.setParameters(parameters);
        scd.setCredentials(bindSecret);
        scd.setDefinition(def);
        return scd;
    }    
    
    private ServiceBinding getServiceBinding(String serviceName) throws IOException {
        ServiceBindingList bindingList = this.scClient.getServiceBindings(ApplicationProperties.getNamespace());
        if (bindingList != null) {
            List<ServiceBinding> bindings = bindingList.getItems();
            if( (bindings != null) && !bindings.isEmpty()) {
                for (ServiceBinding sb : bindings) {
                    LocalObjectReference ref = sb.getSpec().getServiceInstanceRef();
                    if (ref.getName().equals(serviceName)) {
                        return sb;
                    }
                }
            }
        }
        return null;
    }

    private DecodedSecret getParameters(ServiceInstance svc) throws IOException {
        Map<String, String> map = new TreeMap<>();

        // data source is there.
        ParametersFromSource parameters = null;
        for (int i = 0; i < svc.getSpec().getParametersFrom().size(); i++) {
            parameters = svc.getSpec().getParametersFrom().get(i);
            if(parameters.getSecretKeyRef().getKey().equalsIgnoreCase("parameters")) {
                break;
            }
        }
        if (parameters != null) {
            String secretName = parameters.getSecretKeyRef().getName();
            String key = parameters.getSecretKeyRef().getKey();
            Secret secret = this.scClient.getSecret(ApplicationProperties.getNamespace(),
                    secretName);
            if (secret != null) {
                String json = secret.getData().get(key);
                map = new ObjectMapper().readerFor(Map.class).readValue(Base64.getDecoder().decode(json));
            }
            return new DecodedSecret(secretName, map);
        } else {
            KLog.getLogger().debug(svc.getMetadata().getName()+":No Parameters Secret found");
        }
        return null;
    }

    private void createDataSource(String name, DefaultServiceCatalogDataSource scd)
            throws AdminException, KException {
        
        KLog.getLogger().debug("Creating the Datasource = "+ name + " of Type " + scd.getType());

        String driverName = null;
        Set<String> templateNames = this.metadata.admin().getDataSourceTemplateNames();
        KLog.getLogger().debug("template names:"+templateNames);
        String dsType = scd.getDefinition().getType();
        for (String template : templateNames) {
            // TODO: there is null entering from above call from getDataSourceTemplateNames need to investigate why
            if ((template != null) && template.contains(dsType)) {
                driverName = template;
                break;
            }
        }

        if (driverName == null) {
            throw new KException("No driver or resource adapter found for source type " + dsType);
        }

        Properties properties = scd.convertToDataSourceProperties();
        this.metadata.admin().createDataSource(name, driverName, properties);
    }


    private ImageStream createImageStream(OpenShiftClient client, String namespace, String vdbName) {
        ImageStream is = client.imageStreams().inNamespace(namespace).createOrReplaceWithNew()
            .withNewMetadata().withName(vdbName).addToLabels("application", vdbName).endMetadata()
            .done();
        return is;
    }

    private ObjectReference baseImage(OpenShiftClient client, PublishConfiguration publishConfiguration) throws KException {
        ObjectReference fromImage = publishConfiguration.getBaseJDKImage(client);
        if (fromImage == null) {
            throw new KException("Build can not be started as there are no JDK base images availble. "
                    + "Make sure 'redhat-openjdk18-openshift' image stream is available");
        }
        return fromImage;
    }

    private BuildConfig createBuildConfig(OpenShiftClient client, String namespace, String vdbName, ImageStream is,
            PublishConfiguration publishConfiguration) throws KException {
        String imageStreamName = is.getMetadata().getName()+":latest";
        ObjectReference fromImage = baseImage(client, publishConfiguration);
        BuildConfig bc = client.buildConfigs().inNamespace(namespace).createOrReplaceWithNew()
            .withNewMetadata().withName(getBuildConfigName(vdbName))
                .addToLabels("application", vdbName)
                .addToLabels(MANAGED_BY, DAS)
                .endMetadata()
            .withNewSpec()
                .withRunPolicy("SerialLatestOnly")
                .withNewSource().withType("Binary").endSource()
                .withNewStrategy()
                .withType("Source").withNewSourceStrategy()
                .withFrom(fromImage)
                .withIncremental(false)
                .withEnv(new EnvVar("AB_JOLOKIA_OFF", "true", null), new EnvVar("AB_OFF", "true", null))
                .endSourceStrategy()
                .endStrategy()
                .withNewOutput()
                    .withNewTo().withKind("ImageStreamTag").withName(imageStreamName).endTo()
                .endOutput()
            .endSpec()
            .done();
        return bc;
    }

    private String getBuildConfigName(String vdbName) {
        return vdbName+"-build-config";
    }

    private Build createBuild(OpenShiftClient client, String namespace, BuildConfig config,
            InputStream tarInputStream) {
        Build build = client.buildConfigs()
                .inNamespace(namespace)
                .withName(config.getMetadata().getName())
                .instantiateBinary().fromInputStream(tarInputStream);
        return build;
    }

    private DeploymentConfig createDeploymentConfig(OpenShiftClient client, BuildStatus config) {
        
        String readinessPayload = "-d '{\"operation\": \"execute-query\", "
                + "\"vdb-name\": \""+config.vdbName+"\","
                + "\"vdb-version\": \"1.0.0\", "
                + "\"sql-query\": \"select 1\", "
                + "\"timeout-in-milli\": 100, "
                + "\"address\": [\"subsystem\",\"teiid\"], "
                + "\"json.pretty\":1}'";
        
        String livenessPayload = "-d '{\"operation\": \"get-vdb\", "
                + "\"vdb-name\": \""+config.vdbName+"\","
                + "\"vdb-version\": \"1.0.0\", "
                + "\"address\": [\"subsystem\",\"teiid\"], "
                + "\"json.pretty\":1}'";
        
        return client.deploymentConfigs().inNamespace(config.namespace).createOrReplaceWithNew()
            .withNewMetadata().withName(config.vdbName)
                .addToLabels("application", config.vdbName)
            .endMetadata()
            .withNewSpec()
              .withReplicas(1)
              .withNewStrategy().withType("Recreate").endStrategy()
              .addNewTrigger()
                .withType("ConfigChange")
                .withType("ImageChange")
                    .withNewImageChangeParams()
                        .withAutomatic(true)
                        .addToContainerNames(config.vdbName)
                        .withNewFrom().withKind("ImageStreamTag").withName(config.vdbName+":latest").endFrom()
                    .endImageChangeParams()
              .endTrigger()
              .addToSelector("deploymentConfig", config.vdbName)
              .withNewTemplate()
                .withNewMetadata()
                  .withName(config.vdbName)
                  .addToLabels("application", config.vdbName)
                  .addToLabels("deploymentConfig", config.vdbName)
                .endMetadata()
                .withNewSpec()
                  .addNewContainer()
                    .withName(config.vdbName)
                    .withImage(" ")
                    .withImagePullPolicy("Always")
                    .addAllToEnv(config.publishConfiguration.allEnvironmentVariables)
                    .withNewReadinessProbe()
                      .withNewExec()
                        .withCommand("/bin/sh", "-i", "-c", 
                                "curl -X POST " + readinessPayload + CONTENT_TYPE + MANAGEMENT_URL 
                                + " | grep '\"outcome\" : \"success\"'")
                      .endExec()
                      .withInitialDelaySeconds(30)
                      .withTimeoutSeconds(80)
                      .withPeriodSeconds(60)
                      .withFailureThreshold(5)
                      .withSuccessThreshold(1)                      
                    .endReadinessProbe()
                    .withNewLivenessProbe()
                      .withNewExec()
                        .withCommand("/bin/sh", "-i", "-c", 
                              "curl -X POST " + livenessPayload + CONTENT_TYPE + MANAGEMENT_URL 
                              + " | grep '\"outcome\" : \"success\"'")
                      .endExec()
                      .withInitialDelaySeconds(30)
                      .withTimeoutSeconds(80)
                      .withPeriodSeconds(60)
                      .withFailureThreshold(5)
                      .withSuccessThreshold(1)
                    .endLivenessProbe()
                    .withNewResources()
                      .addToLimits("memory", new Quantity(config.publishConfiguration.containerMemorySize))
                      .addToLimits("cpu", new Quantity(config.publishConfiguration.cpuUnits()))
                    .endResources()
                    .addAllToPorts(getDeploymentPorts(config.publishConfiguration))
                  .endContainer()
                .endSpec()
              .endTemplate()
            .endSpec()
            .done();
    }

    private List<ContainerPort> getDeploymentPorts(PublishConfiguration config){
        List<ContainerPort> ports = new ArrayList<>();
        ports.add(createPort(ProtocolType.JOLOKIA.id(), 8778, "TCP"));
        ports.add(createPort(ProtocolType.JDBC.id(), 31000, "TCP"));
        ports.add(createPort(ProtocolType.ODBC.id(), 35432, "TCP"));
        if (config.enableOdata) {
            ports.add(createPort(ProtocolType.ODATA.id(), 8080, "TCP"));
            ports.add(createPort(ProtocolType.SODATA.id(), 8443, "TCP"));
        }
        return ports;
    }
    
    private ContainerPort createPort(String name, int port, String protocol) {
        ContainerPort p = new ContainerPort();
        p.setName(name);
        p.setContainerPort(port);
        p.setProtocol(protocol);
        return p;
    }
    
    private Service createService(OpenShiftClient client, String namespace, String vdbName, String type, int port) {
        String serviceName = vdbName+"-"+type;
        Service service = client.services().inNamespace(namespace).withName(serviceName).get();
        if (service == null) {
            service = client.services().inNamespace(namespace).createNew()
              .withNewMetadata()
                .withName(serviceName)
                .addToLabels("application", vdbName)
                .addToAnnotations("description", "Virtual Database (VDB)")
              .endMetadata()
              .withNewSpec()
                .addToSelector("application", vdbName)
                .addNewPort()
                  .withName(type)
                  .withPort(port)
                  .withNewTargetPort()
                    .withStrVal(type)
                  .endTargetPort()
                .endPort()
              .endSpec()
              .done();
        }
        return service;
    }

    private Route createRoute(OpenShiftClient client, String namespace, String vdbName, String type) {
        String routeName = vdbName+"-"+type;
        Route route = client.routes().inNamespace(namespace).withName(routeName).get();
        if (route == null) {
            route = client.routes().inNamespace(namespace).createNew()
              .withNewMetadata()
                .withName(routeName)
                .addToLabels("application", vdbName)
                .addToAnnotations("description", "Virtual Database (VDB)")
              .endMetadata()
              .withNewSpec()
              .withNewPort().withNewTargetPort().withStrVal(type).endTargetPort().endPort()
              .withNewTo().withName(routeName).endTo()
              .endSpec()
              .done();
        }
        return route;
    }

    private void waitUntilPodIsReady(final OpenShiftClient client, String podName, int nAwaitTimeout) {
        final CountDownLatch readyLatch = new CountDownLatch(1);
        try (Watch watch = client.pods().withName(podName).watch(new Watcher<Pod>() {
            @Override
            public void eventReceived(Action action, Pod aPod) {
                if(KubernetesHelper.isPodReady(aPod)) {
                    readyLatch.countDown();
                }
            }
            @Override
            public void onClose(KubernetesClientException e) {
                // Ignore
            }
        })) {
            readyLatch.await(nAwaitTimeout, TimeUnit.SECONDS);
        } catch (KubernetesClientException | InterruptedException e) {
            KLog.getLogger().error("Could not watch pod", e);
        }
    }

    private void createServices(final OpenShiftClient client, final String namespace,
            final String vdbName) {
        createService(client, namespace, vdbName, ProtocolType.ODATA.id(), 8080);
        createService(client, namespace, vdbName, ProtocolType.JDBC.id(), 31000);
        createService(client, namespace, vdbName, ProtocolType.ODBC.id(), 35432);
        createRoute(client, namespace, vdbName, ProtocolType.ODATA.id());
        //createRoute(client, namespace, vdbName, RouteType.JDBC.id());
    }

    private boolean isDeploymentInReadyState(DeploymentConfig dc) {
        List<DeploymentCondition> conditions = dc.getStatus().getConditions();
        for (DeploymentCondition cond : conditions) {
            if (cond.getType().equals("Available") && cond.getStatus().equals("True")) {
                return true;
            }
        }
        return false;
    }

    private DeploymentCondition getDeploymentConfigStatus(DeploymentConfig dc) {
        List<DeploymentCondition> conditions = dc.getStatus().getConditions();
        for (DeploymentCondition cond : conditions) {
            if (cond.getType().equals("Available")) {
                return cond;
            }
        }
        return null;
    }

    private void addToQueue(final String namespace, final String vdbName, String buildName, String deployConfigName,
            PublishConfiguration publishConfig, Collection<EnvVar> envs) {
        BuildStatus work = new BuildStatus();
        work.buildName = buildName;
        work.status = Status.BUILDING;
        work.vdbName = vdbName;
        work.namespace = namespace;
        work.deploymentName = deployConfigName;
        work.statusMessage = "Build Running";
        work.lastUpdated = System.currentTimeMillis();
        work.publishConfiguration = publishConfig;
        work.publishConfiguration.addEnvironmentVariables(envs);
        this.workQueue.add(work);
    }

    private void monitorWork() {
        if (running) {
            return;
        }

        ExecutorService threadService = Executors.newFixedThreadPool(1);
        threadService.execute(new Runnable() {
        @Override
        public void run() {
            Config config = new ConfigBuilder().build();
            KubernetesClient kubernetesClient = new DefaultKubernetesClient(config);
            final OpenShiftClient client = kubernetesClient.adapt(OpenShiftClient.class);

            try {
                // as along there are
                running = true;
                while(!workQueue.isEmpty()) {
                    BuildStatus work = workQueue.peek();
                    if (work == null) {
                        debug("Publishing - No build in the build queue");
                        break;
                    }

                    // introduce some delay..
                    if ((System.currentTimeMillis()-work.lastUpdated) < 3000) {
                        try {
                            Thread.sleep(3000);
                        } catch (InterruptedException e) {
                            break;
                        }
                    }

                    Build build = client.builds().inNamespace(work.namespace).withName(work.buildName).get();
                    if (build == null) {
                        // build got deleted some how ignore..
                        debug("Publishing " + work.vdbName + " - No build available for building");
                        continue;
                    }

                    boolean shouldReQueue = true;
                    String lastStatus = build.getStatus().getPhase();
                    if (Builds.isCompleted(lastStatus)) {
                        debug("Publishing " + work.vdbName + " - Build completed. Preparing to deploy");

                        work.statusMessage = "build completed, deployment started";
                        if (work.deploymentName == null) {
                            DeploymentConfig dc = createDeploymentConfig(client, work);
                            work.deploymentName = dc.getMetadata().getName();
                            work.status = Status.DEPLOYING;
                            client.deploymentConfigs()
                                .inNamespace(work.namespace)
                                .withName(dc.getMetadata().getName())
                                .deployLatest();
                        } else {
                            DeploymentConfig dc = client.deploymentConfigs().inNamespace(work.namespace)
                                    .withName(work.deploymentName).get();
                            if (isDeploymentInReadyState(dc)) {
                                // it done now..
                                debug("Publishing " + work.vdbName + " - Deployment completed");
                                createServices(client, work.namespace, work.vdbName);
                                work.status = Status.RUNNING;
                                shouldReQueue = false;
                            } else {
                                debug("Publishing " + work.vdbName + " - Deployment invalid");
                                DeploymentCondition cond = getDeploymentConfigStatus(dc);
                                if (cond != null) {
                                    work.statusMessage = cond.getMessage();
                                } else {
                                    work.statusMessage = "Available condition not found in the Deployment Config";
                                }
                            }
                        }
                    } else if (Builds.isCancelled(lastStatus)) {
                        debug("Publishing " + work.vdbName + " - Build cancelled");
                        // once failed do not queue the work again.
                        shouldReQueue = false;
                        work.status = Status.CANCELLED;
                        work.statusMessage = build.getStatus().getMessage();
                        KLog.getLogger().debug("Build cancelled :" + work.buildName + ". Reason "
                                + build.getStatus().getLogSnippet());
                    } else if (Builds.isFailed(lastStatus)) {
                        debug("Publishing " + work.vdbName + " - Build failed");
                        // once failed do not queue the work again.
                        shouldReQueue = false;
                        work.status = Status.FAILED;
                        work.statusMessage = build.getStatus().getMessage();
                        KLog.getLogger().debug("Build failed :" + work.buildName + ". Reason "
                                + build.getStatus().getLogSnippet());
                    }

                    synchronized (work) {
                        work.lastUpdated = System.currentTimeMillis();
                        workQueue.poll(); // remove
                        if (shouldReQueue) {
                            workQueue.offer(work); // add at end
                        }
                    }
                }
            } finally {
                running = false;
                kubernetesClient.close();
            }
        }});
    }

    /**
     * Publish the vdb as a virtualization
     *
     * @param authToken the authentication token for Openshift
     * @param uow the transaction for accessing the vdb's properties
     * @param vdb the vdb for virtualising
     * @return the build status of the virtualization
     * @throws KException if error occurs
     */
    public BuildStatus publishVirtualization(AuthToken authToken, UnitOfWork uow, PublishConfiguration publishConfig) throws KException {
        String namespace = ApplicationProperties.getNamespace();
        Config config = new ConfigBuilder().build();
        OpenShiftConfig.wrap(config).setBuildTimeout(publishConfig.buildTimeoutInSeconds);
        KubernetesClient kubernetesClient = new DefaultKubernetesClient(config);
        final OpenShiftClient client = kubernetesClient.adapt(OpenShiftClient.class);

        Vdb vdb = publishConfig.vdb;
        String vdbName = vdb.getVdbName(uow);
        try {
            debug("Publishing (" + vdbName + ") - Start publishing of virtualization: " + vdbName);

            BuildStatus status = getVirtualizationStatus(vdbName);
            debug("Publishing (" + vdbName + ") - Virtualisation status: " + status.status);

            if ((status.status == Status.BUILDING) || (status.status == Status.DEPLOYING)
                    || (status.status == Status.RUNNING)) {
                return status;
            } else {
                logger.info("Deploying " + vdbName + "as Service");

                debug("Publishing (" + vdbName + ") - Checking for base image");
                baseImage(client, publishConfig);

                // create build contents as tar file

                debug("Publishing (" + vdbName + ") - Creating zip archive");
                GenericArchive archive = ShrinkWrap.create(GenericArchive.class, "contents.tar");
                String pomFile = generatePomXml(authToken, uow, vdb, publishConfig.enableOdata);

                debug("Publishing (" + vdbName + ") - Generated pom file: " + NEW_LINE + pomFile);
                archive.add(new StringAsset(pomFile), "pom.xml");

                byte[] vdbFile = vdb.export(uow, null);
                debug("Publishing (" + vdbName + ") - Exported vdb: " + NEW_LINE + new String(vdbFile));
                archive.add(new ByteArrayAsset(vdbFile), "/src/main/vdb/"+vdbName+"-vdb.xml");

                InputStream configIs = this.getClass().getClassLoader().getResourceAsStream("s2i/project-defaults.yml");
                archive.add(new ByteArrayAsset(ObjectConverterUtil.convertToByteArray(configIs)),
                        "/src/main/resources/project-defaults.yml");

                debug("Publishing (" + vdbName + ") - Converting archive to TarExport");
                InputStream buildContents = archive.as(TarExporter.class).exportAsInputStream();                
                debug("Publishing (" + vdbName + ") - Completed creating build contents construction");

                debug("Publishing (" + vdbName + ") - Creating image stream");
                // use the contents to invoke a binary build
                ImageStream is = createImageStream(client, namespace, vdbName);

                debug("Publishing (" + vdbName + ") - Creating build config");
                BuildConfig buildConfig = createBuildConfig(client, namespace, vdbName, is, publishConfig);

                debug("Publishing (" + vdbName + ") - Creating build");
                Build build = createBuild(client, namespace, buildConfig, buildContents);
                KLog.getLogger().info("Build Started:"+build.getMetadata().getName()+" for VDB "+ vdbName + " to publish");

                debug("Publishing (" + vdbName + ") - Awaiting pod readiness ...");
                waitUntilPodIsReady(client, build.getMetadata().getName() + "-build", 20);

                debug("Publishing (" + vdbName + ") - Fetching environment variables for vdb data sources");
                Collection<EnvVar> envs = getEnvironmentVariablesForVDBDataSources(authToken, uow, vdb, publishConfig);

                debug("Publishing (" + vdbName + ") - Adding to queue");
                addToQueue(namespace, vdbName, build.getMetadata().getName(), null, publishConfig, envs);
            }

            debug("Publishing (" + vdbName + ") - Initiating work monitor if not already running");
            monitorWork();

            BuildStatus virtualizationStatus = getVirtualizationStatus(vdbName);
            debug("Published (" + vdbName + ") - Status of build + " + virtualizationStatus.buildName + 
                         COLON + SPACE + virtualizationStatus.status);
            return virtualizationStatus;

        } catch (KubernetesClientException | IOException e) {
            throw new KException(e);
        } finally {
            kubernetesClient.close();
        }
    }

    Collection<EnvVar> getEnvironmentVariablesForVDBDataSources(AuthToken authToken, UnitOfWork uow, Vdb vdb,
            PublishConfiguration publishConfig) throws KException {
        List<EnvVar> envs = new ArrayList<>();
        StringBuilder javaOptions = new StringBuilder();
        Model[] models = vdb.getModels(uow);
        for (Model model : models) {
            ModelSource[] sources = model.getSources(uow);
            for (ModelSource source : sources) {
                String name = source.getName(uow);
                String translatorName = source.getTranslatorName(uow);
                DataSourceDefinition def = getSourceDefinitionThatMatchesTranslator(translatorName);
                DefaultServiceCatalogDataSource ds = null;
                if (def.isServiceCatalogSource()) {
                    ds = getServiceCatalogDataSource(authToken, name);
                    if (ds == null) {
                        throw new KException("Datasource "+name+" not found service catalog");
                    }
    
                    // if null this is either file, ws, kind of source where service catalog source does not exist
                    def = ds.getDefinition();
                    if (def == null) {
                        throw new KException("Failed to determine the source type for "
                                + name + " in VDB " + vdb.getName(uow));
                    }
                } else {
                    ds = new DefaultServiceCatalogDataSource();
                    ds.setName(name);
                    ds.setTranslatorName(translatorName);
                    ds.setDefinition(def);
                }

                //  build properties to create data source in WF-SWARM
                convertSecretsToEnvironmentVariables(ds, envs);

                Properties config = def.getWFSDataSourceProperties(ds, source.getJndiName(uow));
                if (config != null) {
                    for (String key : config.stringPropertyNames()) {
                        javaOptions.append(" -D").append(key).append("=").append(config.getProperty(key));
                    }
                }
            }
        }
        // These options need to be removed after the base image gets updated with them natively
        javaOptions.append(publishConfig.getUserJavaOptions());
        envs.add(env("JAVA_OPTIONS", javaOptions.toString()));
        for (Map.Entry<String, String> entry : publishConfig.getUserEnvironmentVariables().entrySet()) {
            envs.add(env(entry.getKey(), entry.getValue()));
        }
        return envs;
    }

    /**
     * Convert given secrets for Service Catalog data source into Environment variables.
     * @param datasource data source
     * @return YML fragment depicting the ENV variables
     */
    private List<EnvVar> convertSecretsToEnvironmentVariables(DefaultServiceCatalogDataSource datasource,
            List<EnvVar> envs) {
        if (datasource.getParameters() != null) {
            for (String key : datasource.getParameters().getData().keySet()) {
                String instanceKey = datasource.getParameters().canonicalKey(key);
                envs.add(env(instanceKey, datasource.getParameters().getData().get(key)));
            }
        }
        if (datasource.getCredentials() != null) {
            for (String key : datasource.getCredentials().getData().keySet()) {
                String instanceKey = datasource.getCredentials().canonicalKey(key);
                envs.add(env(instanceKey, datasource.getCredentials().getSecretName(), key));
            }
        }
        return envs;
    }

    protected EnvVar env(String name, String secretName, String key) {
        return new EnvVarBuilder().withName(name).withNewValueFrom().withNewSecretKeyRef().withName(secretName)
                .withKey(key).endSecretKeyRef().endValueFrom().build();
    }

    protected EnvVar env(String name, String value) {
        return new EnvVarBuilder().withName(name).withValue(value).build();
    }

    public BuildStatus getVirtualizationStatus(String vdbName) {
        for (BuildStatus status: workQueue) {
            if (status.vdbName.equals(vdbName)) {
                return status;
            }
        }
        String namespace = ApplicationProperties.getNamespace();
        Config config = new ConfigBuilder().build();
        KubernetesClient kubernetesClient = new DefaultKubernetesClient(config);
        final OpenShiftClient client = kubernetesClient.adapt(OpenShiftClient.class);
        try {
            return getVDBSerice(vdbName, namespace, client);
        } finally {
            kubernetesClient.close();
        }
    }

    private BuildStatus getVDBSerice(String vdbName, String namespace, final OpenShiftClient client) {
        BuildStatus status = new BuildStatus();
        status.vdbName = vdbName;
        status.namespace = namespace;

        BuildConfig buildConfig = client.buildConfigs().inNamespace(namespace).withName(getBuildConfigName(vdbName)).get();
        if (buildConfig != null) {
            BuildList buildList = client.builds().inNamespace(namespace).withLabel("application", vdbName).list();
            if ((buildList !=null) && !buildList.getItems().isEmpty()) {
                Build build = buildList.getItems().get(0);
                status.buildName = build.getMetadata().getName();
                if (Builds.isCancelled(build.getStatus().getPhase())) {
                    status.status = Status.CANCELLED;
                    status.statusMessage = build.getStatus().getMessage();
                } else if (Builds.isFailed(build.getStatus().getPhase())) {
                    status.status = Status.FAILED;
                    status.statusMessage = build.getStatus().getMessage();
                } else if (Builds.isCompleted(build.getStatus().getPhase())) {
                    DeploymentConfig dc = client.deploymentConfigs().inNamespace(namespace).withName(vdbName).get();
                    if (dc != null) {
                        status.status = Status.DEPLOYING;
                        status.deploymentName = dc.getMetadata().getName();
                        if (isDeploymentInReadyState(dc)) {
                            status.status = Status.RUNNING;

                            //
                            // Only if status is running then populate the routes
                            // for this virtualization
                            //
                            ProtocolType[] types = { ProtocolType.ODATA, ProtocolType.JDBC, ProtocolType.ODBC };
                            for (ProtocolType type : types) {
                                RouteStatus route = getRoute(vdbName, type);
                                if (route == null)
                                    continue;

                                status.addRoute(route);
                            }
                        }

                        DeploymentCondition cond = getDeploymentConfigStatus(dc);
                        if (cond != null) {
                            status.statusMessage = cond.getMessage();
                        } else {
                            status.statusMessage = "Available condition not found in deployment, delete the service and re-deploy?";
                        }
                    } else {
                        status.statusMessage = "Build Completed, but no deployment found. Reason unknown, please redeploy";
                        status.status = Status.FAILED;
                    }
                } else {
                    status.status = Status.BUILDING;
                    status.statusMessage = build.getStatus().getMessage();
                }
            }
        }
        status.lastUpdated = System.currentTimeMillis();
        return status;
    }

    public List<BuildStatus> getVirtualizations(boolean includeInQueue){
        String namespace = ApplicationProperties.getNamespace();
        Config config = new ConfigBuilder().build();
        KubernetesClient kubernetesClient = new DefaultKubernetesClient(config);
        final OpenShiftClient client = kubernetesClient.adapt(OpenShiftClient.class);
        ArrayList<BuildStatus> services = new ArrayList<>();
        try {
            BuildList bl = client.builds().inNamespace(namespace).withLabel(MANAGED_BY, DAS).list();
            for (Build b : bl.getItems()) {
                String vdbName = b.getMetadata().getLabels().get("application");
                services.add(getVDBSerice(vdbName, namespace, client));
            }
        } finally {
            kubernetesClient.close();
        }

        if (includeInQueue) {
            services.addAll(workQueue);
        }
        return services;
    }

    public BuildStatus deleteVirtualization(String vdbName) {
        BuildStatus runningBuild = null;
        for (BuildStatus status: workQueue) {
            if (status.vdbName.equals(vdbName)) {
                runningBuild = status;
                workQueue.remove(status);
            }
        }
        String namespace = ApplicationProperties.getNamespace();
        Config config = new ConfigBuilder().build();
        KubernetesClient kubernetesClient = new DefaultKubernetesClient(config);
        final OpenShiftClient client = kubernetesClient.adapt(OpenShiftClient.class);
        try {
            KLog.getLogger().info("Deleting the " + vdbName + "that is deployed as Service");
            if (runningBuild != null) {
                client.builds().inNamespace(runningBuild.namespace).withName(runningBuild.buildName).delete();
            } else {
                runningBuild = getVirtualizationStatus(vdbName);
            }
            client.buildConfigs().inNamespace(namespace).withName(getBuildConfigName(vdbName)).delete();
            client.deploymentConfigs().inNamespace(namespace).withLabel("application", vdbName).delete();
            //client.routes().inNamespace(namespace).withName(vdbName+HYPHEN + RouteType.JDBC.id()).delete();
            client.routes().inNamespace(namespace).withName(vdbName+HYPHEN + ProtocolType.ODATA.id()).delete();
            client.services().inNamespace(namespace).withName(vdbName+HYPHEN + ProtocolType.JDBC.id()).delete();
            client.services().inNamespace(namespace).withName(vdbName+HYPHEN + ProtocolType.ODATA.id()).delete();
            client.services().inNamespace(namespace).withName(vdbName+HYPHEN + ProtocolType.ODBC.id()).delete();
            client.imageStreams().inNamespace(namespace).withName(vdbName).delete();
        } finally {
            kubernetesClient.close();
        }
        runningBuild.statusMessage = "deleted";
        return runningBuild;
    }

    private RouteStatus getRoute(String vdbName, ProtocolType protocolType) {
        String namespace = ApplicationProperties.getNamespace();
        Config config = new ConfigBuilder().build();
        KubernetesClient kubernetesClient = new DefaultKubernetesClient(config);
        final OpenShiftClient client = kubernetesClient.adapt(OpenShiftClient.class);
        try {
            RouteStatus theRoute = null;
            KLog.getLogger().info("Getting route of type " + protocolType.id() + " for " + vdbName + " Service");
            RouteList routes = client.routes().inNamespace(namespace).list();
            if (routes == null || routes.getItems().isEmpty())
                return theRoute;

            for (Route route : routes.getItems()) {
                ObjectMeta metadata = route.getMetadata();
                String name = metadata.getName();
                if (! name.endsWith(HYPHEN + protocolType.id()))
                    continue;

                theRoute = new RouteStatus(name, protocolType);

                RouteSpec spec = route.getSpec();
                theRoute.setHost(spec.getHost());
                theRoute.setPath(spec.getPath());
                theRoute.setPort(spec.getPort().getTargetPort().getStrVal());
                theRoute.setTarget(spec.getTo().getName());
            }

            return theRoute;

        } finally {
            kubernetesClient.close();
        }
    }

    /**
     * This method generates the pom.xml file, that needs to be saved in the root of the project.
     * @param authToken - token for Openshift authentication
     * @param uow - Unit Of Work
     * @param vdb - VDB for which pom.xml is generated
     * @return pom.xml contents
     * @throws KException
     */
    protected String generatePomXml(AuthToken authToken, UnitOfWork uow, Vdb vdb, boolean enableOdata) throws KException {
        try {
            StringBuilder builder = new StringBuilder();
            InputStream is = this.getClass().getClassLoader().getResourceAsStream("s2i/template-pom.xml");
            builder.append(new String(ObjectConverterUtil.convertToByteArray(is)));

            StringBuilder vdbSourceNames = new StringBuilder();
            StringBuilder vdbDependencies = new StringBuilder();

            String vdbName = vdb.getName(uow);
            Model[] models = vdb.getModels(uow);
            for (Model model : models) {
                ModelSource[] sources = model.getSources(uow);
                for (ModelSource source : sources) {
                    String name = source.getName(uow);
                    String translatorName = source.getTranslatorName(uow);
                    DefaultServiceCatalogDataSource ds = getServiceCatalogDataSource(authToken, name);
                    if (ds == null) {
                        throw new KException("Datasource " + name + " not found in the service catalog");
                    }
                    DataSourceDefinition def = ds.getDefinition(); 
                    if (def == null) {
                        def = getSourceDefinitionThatMatchesTranslator(translatorName);
                    }
                    
                    vdbSourceNames.append(name).append(StringConstants.SPACE); // this used as label
                    vdbDependencies.append(def.getPomDendencies());
                    vdbDependencies.append(StringConstants.NEW_LINE);
                }
            }

            if (enableOdata) {
                vdbDependencies.append(StringConstants.NEW_LINE).append("<dependency>"
                        + "<groupId>org.wildfly.swarm</groupId>"
                        + "<artifactId>odata</artifactId>"
                        + "</dependency> ");
            }

            String pomXML = builder.toString();
            pomXML = pomXML.replace("<!--vdb-name-->", vdbName);
            pomXML = pomXML.replace("<!--vdb-source-names-->", vdbSourceNames.toString());
            pomXML = pomXML.replace("<!--vdb-dependencies-->", vdbDependencies.toString());
            return pomXML;
        } catch (IOException e) {
            throw handleError(e);
        }
    }

    protected static KException handleError(Throwable e) {
        assert (e != null);

        if (e instanceof KException) {
            return (KException)e;
        }

        return new KException(e);
    }
}
