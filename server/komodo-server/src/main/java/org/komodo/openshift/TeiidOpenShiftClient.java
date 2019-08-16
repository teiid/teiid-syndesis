/*
 * Copyright Red Hat, Inc. and/or its affiliates
 * and other contributors as indicated by the @author tags and
 * the COPYRIGHT.txt file distributed with this work.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 * http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
package org.komodo.openshift;

import java.io.BufferedWriter;
import java.io.ByteArrayInputStream;
import java.io.File;
import java.io.FileWriter;
import java.io.IOException;
import java.io.InputStream;
import java.io.PrintWriter;
import java.io.StringWriter;
import java.io.UnsupportedEncodingException;
import java.security.KeyManagementException;
import java.security.KeyStoreException;
import java.security.NoSuchAlgorithmException;
import java.util.*;
import java.util.concurrent.Callable;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.CountDownLatch;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.LinkedBlockingQueue;
import java.util.concurrent.ThreadPoolExecutor;
import java.util.concurrent.TimeUnit;

import javax.net.ssl.SSLContext;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.apache.http.HttpEntity;
import org.apache.http.HttpResponse;
import org.apache.http.client.ResponseHandler;
import org.apache.http.client.methods.HttpGet;
import org.apache.http.conn.ssl.NoopHostnameVerifier;
import org.apache.http.impl.client.AbstractResponseHandler;
import org.apache.http.impl.client.CloseableHttpClient;
import org.apache.http.impl.client.HttpClients;
import org.apache.http.ssl.SSLContextBuilder;
import org.jboss.shrinkwrap.api.GenericArchive;
import org.jboss.shrinkwrap.api.ShrinkWrap;
import org.jboss.shrinkwrap.api.asset.ByteArrayAsset;
import org.jboss.shrinkwrap.api.asset.StringAsset;
import org.jboss.shrinkwrap.api.exporter.TarExporter;
import org.komodo.KEngine;
import org.komodo.KException;
import org.komodo.StringConstants;
import org.komodo.datasources.*;
import org.komodo.datavirtualization.SourceSchema;
import org.komodo.metadata.MetadataInstance;
import org.komodo.metadata.TeiidDataSource;
import org.komodo.metadata.internal.DefaultMetadataInstance;
import org.komodo.openshift.BuildStatus.RouteStatus;
import org.komodo.openshift.BuildStatus.Status;
import org.komodo.rest.AbstractTransactionService;
import org.komodo.rest.AuthHandlingFilter.OAuthCredentials;
import org.komodo.rest.KomodoConfigurationProperties;
import org.komodo.rest.KomodoService;
import org.komodo.utils.StringNameValidator;
import org.komodo.utils.StringUtils;
import org.teiid.adminapi.AdminException;
import org.teiid.adminapi.Model;
import org.teiid.adminapi.impl.ModelMetaData;
import org.teiid.adminapi.impl.SourceMappingMetadata;
import org.teiid.adminapi.impl.VDBMetaData;
import org.teiid.core.CoreConstants;
import org.teiid.core.util.AccessibleByteArrayOutputStream;
import org.teiid.core.util.ObjectConverterUtil;

import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;

import io.fabric8.kubernetes.api.KubernetesHelper;
import io.fabric8.kubernetes.api.builds.Builds;
import io.fabric8.kubernetes.api.model.*;
import io.fabric8.kubernetes.client.KubernetesClientException;
import io.fabric8.kubernetes.client.Watch;
import io.fabric8.kubernetes.client.Watcher;
import io.fabric8.kubernetes.client.dsl.internal.PodOperationsImpl;
import io.fabric8.openshift.api.model.*;
import io.fabric8.openshift.client.DefaultOpenShiftClient;
import io.fabric8.openshift.client.NamespacedOpenShiftClient;
import io.fabric8.openshift.client.OpenShiftClient;
import io.fabric8.openshift.client.OpenShiftConfig;
import io.fabric8.openshift.client.OpenShiftConfigBuilder;

@SuppressWarnings("nls")
public class TeiidOpenShiftClient extends AbstractTransactionService {
    public static final String ID = "id";
    private static final String SERVICE_CA_CERT_FILE = "/var/run/secrets/kubernetes.io/serviceaccount/service-ca.crt";
    private String openShiftHost = "https://openshift.default.svc";
    private long buildTimeoutInSeconds = 2 * 60 * 1000L;
    private final OpenShiftConfig openShiftClientConfig = new OpenShiftConfigBuilder().withMasterUrl(openShiftHost)
            .withCaCertFile(SERVICE_CA_CERT_FILE).withBuildTimeout(buildTimeoutInSeconds).build();

    private NamespacedOpenShiftClient openshiftClient() {
        return new DefaultOpenShiftClient(openShiftClientConfig);
    }

    /**
     * Responsible for sending SUBMITTED work to be configured
     * and for sending completed builds to be deployed.
     */
    private class BuildStatusRunner implements Runnable {

        private BuildStatus work;

        public BuildStatusRunner(BuildStatus buildStatus) {
            this.work = buildStatus;
        }

        @Override
        public void run() {
            try {
                // introduce some delay..
                long elapsed = System.currentTimeMillis() - work.lastUpdated();
                if (elapsed < 3000) {
                    try {
                        Thread.sleep(3000 - elapsed);
                    } catch (InterruptedException e) {
                        Thread.interrupted();
                        return;
                    }
                }

                if (BuildStatus.Status.DELETE_SUBMITTED.equals(work.status())) {
                    work.setLastUpdated();
                    workExecutor.submit(this); // add at end
                    return;
                }

                if (BuildStatus.Status.DELETE_REQUEUE.equals(work.status())) {
                    // requeue will change state to submitted and
                    work.setLastUpdated();
                    deleteVirtualization(work.vdbName());
                    workExecutor.submit(this); // add at end
                    return;
                }

                if (BuildStatus.Status.DELETE_DONE.equals(work.status())) {
                    return;
                }

                if (BuildStatus.Status.FAILED.equals(work.status()) || BuildStatus.Status.CANCELLED.equals(work.status())) {
                    work.setLastUpdated();
                    return;
                }

                if (BuildStatus.Status.SUBMITTED.equals(work.status())) {
                    //
                    // build submitted for configuration. This is done on another
                    // thread to avoid clogging up the monitor thread.
                    //
                    info(work.vdbName(), "Publishing - Submitted build to be configured");

                    configureBuild(work);

                    work.setLastUpdated();
                    workExecutor.submit(this); // add at end

                    return;
                }

                //
                // build is being configured which is done on another thread
                // so ignore this build for the moment
                //
                if (Status.CONFIGURING.equals(work.status())) {
                    work.setLastUpdated();
                    workExecutor.submit(this); // add at end

                    debug(work.vdbName(), "Publishing - Continuing monitoring as configuring");
                    return;
                }

                boolean shouldReQueue = true;
                try (final OpenShiftClient client = openshiftClient()) {
                    Build build = client.builds().inNamespace(work.namespace()).withName(work.buildName()).get();
                    if (build == null) {
                        // build got deleted some how ignore, remove from monitoring..
                        error(work.vdbName(), "Publishing - No build available for building");
                        return;
                    }

                    String lastStatus = build.getStatus().getPhase();
                    if (Builds.isCompleted(lastStatus)) {
                        if (! Status.DEPLOYING.equals(work.status())) {
                            info(work.vdbName(), "Publishing - Build completed. Preparing to deploy");
                            work.setStatusMessage("build completed, deployment started");
                            createSecret(client, work.namespace(), work.vdbName(), work);
                            DeploymentConfig dc = createDeploymentConfig(client, work);
                            work.setDeploymentName(dc.getMetadata().getName());
                            work.setStatus(Status.DEPLOYING);
                            client.deploymentConfigs().inNamespace(work.namespace())
                                    .withName(dc.getMetadata().getName()).deployLatest();
                        } else {
                            DeploymentConfig dc = client.deploymentConfigs().inNamespace(work.namespace())
                                    .withName(work.deploymentName()).get();
                            if (isDeploymentInReadyState(dc)) {
                                // it done now..
                                info(work.vdbName(), "Publishing - Deployment completed");
                                createServices(client, work.namespace(), work.vdbName());
                                work.setStatus(Status.RUNNING);
                                shouldReQueue = false;
                            } else {
                                if (!isDeploymentProgressing(dc)) {
                                    work.setStatus(Status.FAILED);
                                    info(work.vdbName(), "Publishing - Deployment seems to be failed, this could be "
                                            + "due to vdb failure, rediness check failed. Wait threshold is 2 minutes.");
                                    shouldReQueue = false;
                                }
                                debug(work.vdbName(), "Publishing - Deployment not ready");
                                DeploymentCondition cond = getDeploymentConfigStatus(dc);
                                if (cond != null) {
                                    debug(work.vdbName(), "Publishing - Deployment condition: " + cond.getMessage());
                                    work.setStatusMessage(cond.getMessage());
                                } else {
                                    work.setStatusMessage("Available condition not found in the Deployment Config");
                                }
                            }
                        }
                    } else if (Builds.isCancelled(lastStatus)) {
                        info(work.vdbName(), "Publishing - Build cancelled");
                        // once failed do not queue the work again.
                        shouldReQueue = false;
                        work.setStatus(Status.CANCELLED);
                        work.setStatusMessage(build.getStatus().getMessage());
                        debug(work.vdbName(), "Build cancelled: " + work.buildName() + ". Reason "
                                + build.getStatus().getLogSnippet());
                    } else if (Builds.isFailed(lastStatus)) {
                        error(work.vdbName(), "Publishing - Build failed");
                        // once failed do not queue the work again.
                        shouldReQueue = false;
                        work.setStatus(Status.FAILED);
                        work.setStatusMessage(build.getStatus().getMessage());
                        error(work.vdbName(),
                                "Build failed :" + work.buildName() + ". Reason " + build.getStatus().getLogSnippet());
                    }
                }

                work.setLastUpdated();
                if (shouldReQueue) {
                    workExecutor.submit(this); // add at end
                } else {
                    // Close the log as no longer needed actively
                    closeLog(work.vdbName());
                }
            } catch (Throwable ex) {
                //
                // Does not specify an id so will only be logged in the KLog.
                //
                error(null, "Monitor exception", ex);
            }
        }
    }

    private static final String DESCRIPTION_ANNOTATION_LABEL = "description";

    private static final String SERVICE_DESCRIPTION = "Virtual Database (VDB)";

    private static final Log logger = LogFactory.getLog(TeiidOpenShiftClient.class);

    private static final String SYSDESIS = "syndesis";
    private static final String MANAGED_BY = "managed-by";
    private static final String SYNDESISURL = "http://syndesis-server/api/v1";

    private MetadataInstance metadata;
    private Map<String, DataSourceDefinition> sources = new ConcurrentHashMap<>();

    /**
     * Fixed pool of up to 3 threads for configuring images ready to be deployed
     */
    private ExecutorService configureService = Executors.newFixedThreadPool(3);

    private Map<String, PrintWriter> logBuffers = new ConcurrentHashMap<>();
    private EncryptionComponent encryptionComponent;
    private KomodoConfigurationProperties config;

    private ThreadPoolExecutor workExecutor = new ThreadPoolExecutor(1, 1, 60, TimeUnit.SECONDS, new LinkedBlockingQueue<>());

    public TeiidOpenShiftClient(MetadataInstance metadata, EncryptionComponent encryptor, KomodoConfigurationProperties config, KEngine kengine) {
        this.metadata = metadata;
        this.encryptionComponent = encryptor;
        this.config = config;
        this.kengine = kengine;

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

    private String getLogPath(String id) {
        String parentDir;
        try {
            File loggerPath = File.createTempFile("vdb-", "log");
            parentDir = loggerPath.getParent();
        } catch(Exception ex) {
            logger.error("Failure to get logger path", ex);
            parentDir = System.getProperty(JAVA_IO_TMPDIR);
        }

        return parentDir + File.separator + id + ".log";
    }

    private void closeLog(String id) {
        PrintWriter pw = logBuffers.remove(id);
        if (pw == null)
            return;

        pw.close();
    }
    private void addLog(String id, String message) {
        if (id == null)
            return; // Cannot record these log messages

        try {
            PrintWriter pw = logBuffers.get(id);
            if (pw == null) {
                // No cached buffered writer
                String logPath = getLogPath(id);
                File logFile = new File(logPath);

                FileWriter fw = new FileWriter(logFile, true);
                BufferedWriter bw = new BufferedWriter(fw);
                pw = new PrintWriter(bw);
                logBuffers.put(id, pw);
            }

            Calendar calendar = Calendar.getInstance();
            message =  OPEN_BRACKET + calendar.getTime() + CLOSE_BRACKET + SPACE + HYPHEN + SPACE + message + NEW_LINE;
            pw.write(message);
            pw.flush();

        } catch (Exception ex) {
            error(id, "Error with logging to file", ex);
        }
    }

    private void removeLog(String id) {
        closeLog(id);

        String logPath = getLogPath(id);
        File logFile = new File(logPath);
        if (logFile.exists())
            logFile.delete();
    }

    private void debug(String id, String message) {
        if (! logger.isDebugEnabled())
            return;

        logger.debug(message);
        addLog(id, message);
    }

    private void error(String id, String message, Throwable ex) {
        logger.error(message, ex);
        String cause = StringUtils.exceptionToString(ex);
        addLog(id, message);
        addLog(id,cause);
    }

    private void error(String id, String message) {
        logger.error(message);
        addLog(id, message);
    }

    private void info(String id, String message) {
        logger.info(message);
        addLog(id, message);
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

    private static CloseableHttpClient buildHttpClient()
            throws NoSuchAlgorithmException, KeyManagementException, KeyStoreException {
        // no verification of host for now.
        SSLContext sslContext = new SSLContextBuilder().loadTrustMaterial(null, (certificate, authType) -> true)
                .build();

        CloseableHttpClient client = HttpClients.custom().setSSLContext(sslContext)
                .setSSLHostnameVerifier(new NoopHostnameVerifier()).build();
        return client;
    }

    private static String bearer(String auth) {
        return "Bearer " + auth;
    }

    private static InputStream executeGET(String url, OAuthCredentials oauthCreds) {
        try {
            CloseableHttpClient client = buildHttpClient();
            HttpGet request = new HttpGet(url);
            if (oauthCreds != null) {
                request.addHeader("X-Forwarded-Access-Token", oauthCreds.getToken().toString());
                request.addHeader("X-Forwarded-User", oauthCreds.getUser());
                request.addHeader("Authorization", bearer(oauthCreds.getToken().toString()));
            }

            HttpResponse response = client.execute(request);
            ResponseHandler<InputStream> handler = new AbstractResponseHandler<InputStream>(){
                @Override
                public InputStream handleEntity(final HttpEntity entity) throws IOException {
                    return entity.getContent();
                }
            };
            InputStream result = handler.handleResponse(response);
            return result;
        } catch (UnsupportedOperationException | IOException | KeyManagementException | NoSuchAlgorithmException
                | KeyStoreException e) {
            throw new RuntimeException(e);
        }
    }

    public Set<DefaultSyndesisDataSource> getSyndesisSources(OAuthCredentials oauthCreds) throws KException {
        Set<DefaultSyndesisDataSource> sources = new HashSet<>();
        try {
            String url = SYNDESISURL+"/connections";
            InputStream response = executeGET(url, oauthCreds);
            ObjectMapper mapper = new ObjectMapper();
            JsonNode root = mapper.readTree(response);
            for (JsonNode item: root.get("items")) {
                String connectorType = item.get("connectorId").asText();
                if (!connectorType.equals("sql")) {
                    continue;
                }
                String name = item.get("name").asText();
                try {
                    sources.add(buildSyndesisDataSource(name, item));
                } catch (KException e) {
                    error(name, e.getMessage(), e);
                }
            }
        } catch (Exception e) {
            throw handleError(e);
        }
        return sources;
    }

    public DefaultSyndesisDataSource getSyndesisDataSourceById(OAuthCredentials oauthCreds, String dsId)
            throws KException {
        try {
            String url = SYNDESISURL+"/connections/"+dsId;
            InputStream response = executeGET(url, oauthCreds);
            ObjectMapper mapper = new ObjectMapper();
            JsonNode root = mapper.readTree(response);
            String connectorType = root.get("connectorId").asText();
            if (!connectorType.equals("sql")) {
                throw new KException("Not SQL Connection, not supported yet.");
            }
            String name = root.get("name").asText();
            return buildSyndesisDataSource(name, root);
        } catch (Exception e) {
            throw handleError(e);
        }
    }

    public void bindToSyndesisSource(OAuthCredentials oauthCreds, DefaultSyndesisDataSource scd) throws KException {
        info(scd.getSyndesisName(), "Bind source with name to Service: " + scd.getSyndesisName());
        try {
            String dsName = findDataSourceNameByEventId(scd.getId());
            if (dsName != null) {
                scd.setKomodoName(dsName);
            } else {
                createDataSource(scd);
            }

        } catch (Exception e) {
            throw handleError(e);
        }
    }

    public DefaultSyndesisDataSource getSyndesisDataSource(OAuthCredentials oauthCreds, String dsName)
            throws KException {
        try {
            Set<DefaultSyndesisDataSource> sources = getSyndesisSources(oauthCreds);
            for (DefaultSyndesisDataSource source:sources) {
                if (dsName.equals(source.getKomodoName())) {
                    return (DefaultSyndesisDataSource)source;
                }
            }
            return null;
        } catch (Exception e) {
            throw handleError(e);
        }
    }

    private DefaultSyndesisDataSource buildSyndesisDataSource(String syndesisName, JsonNode item)
            throws KException {
        Map<String, String> p = new HashMap<>();
        JsonNode configuredProperties = item.get("configuredProperties");
        JsonNode connectorIDNode = item.get(ID);
        configuredProperties.fieldNames()
                .forEachRemaining(key -> p.put(key, configuredProperties.get(key).asText()));

        DataSourceDefinition def = getSourceDefinitionThatMatches(p);
        if (def != null) {
            if( connectorIDNode != null ) {
                DefaultSyndesisDataSource dsd = new DefaultSyndesisDataSource();
                dsd.setId(connectorIDNode.asText());
                dsd.setSyndesisName(syndesisName);
                String dsName = findDataSourceNameByEventId(connectorIDNode.asText());
                dsd.setKomodoName(dsName);
                dsd.setTranslatorName(def.getTranslatorName());
                dsd.setProperties(p);
                dsd.setDefinition(def);
                return dsd;
            } else {
                throw new KException("Datasource has no connection ID");
            }
        } else {
            throw new KException("Could not find datasource that matches to the configuration."+ p.get("url"));
        }
    }

    private void createDataSource(DefaultSyndesisDataSource scd)
            throws Exception {
        String syndesisName = scd.getSyndesisName();
        debug(syndesisName, "Creating the Datasource of Type " + scd.getType());

        Set<String> templateNames = this.metadata.getDataSourceTemplateNames();
        debug(syndesisName, "template names: " + templateNames);
        String dsType = scd.getType();

        //we'll create serially to ensure a unique generated name
        setUniqueKomodoName(scd, syndesisName, KomodoService.SYSTEM_USER_NAME);
        String toUse = scd.getKomodoName();

        //now that the komodoname is set, we can create the properties
        Map<String, String> properties = scd.convertToDataSourceProperties();
        properties.put(ID, scd.getId());

        this.metadata.createDataSource(toUse, dsType, encryptionComponent.decrypt(properties));
    }

    /**
     * Create a unique and valid name the syndesis connection.  The name will be suitable
     * as a schema name as well.
     * @param scd
     * @param syndesisName
     * @throws Exception
     */
    public void setUniqueKomodoName(DefaultSyndesisDataSource scd, String syndesisName, String user) throws Exception {
        runInTransaction(user, "setUniqueKomodoName", false, () -> {
            SourceSchema ss = kengine.getWorkspaceManager().findSchema(scd.getId());
            if (ss != null) {
                //just reassociate
                scd.setKomodoName(ss.getName());
                return null;
            }

            String name = syndesisName;
            int maxLength = StringNameValidator.DEFAULT_MAXIMUM_LENGTH;
            //remove any problematic characters
            name = name.replaceAll("[\\.\\?\\_\\s]", "");
            //slim it down
            if (name.length() > maxLength) {
                name = name.substring(0, maxLength);
            }

            TreeSet<String> taken = new TreeSet<>(String.CASE_INSENSITIVE_ORDER);
            taken.addAll(kengine.getWorkspaceManager().findAllSchemaNames());

            //TODO: drive this via an api method
            taken.add(CoreConstants.INFORMATION_SCHEMA);
            taken.add(CoreConstants.ODBC_MODEL);
            taken.add(CoreConstants.SYSTEM_ADMIN_MODEL);
            taken.add(CoreConstants.SYSTEM_MODEL);

            taken.add(SERVICE_VDB_VIEW_MODEL);

            int i = 1;
            String toUse = name;
            while (taken.contains(toUse)) {
                if (name.length() + (i/10 + 1) > maxLength) {
                    name = name.substring(0, maxLength - (i/10 + 1));
                }
                toUse = name + i;
                i++;
            }

            scd.setKomodoName(toUse);
            //update the db with the name we'll use
            kengine.getWorkspaceManager().createOrUpdateSchema(scd.getId(), toUse, null);
            return null;
        });
    }

    public void deleteDataSource(String dsName) throws AdminException, KException {
        this.metadata.deleteDataSource(dsName);
    }

    public String findDataSourceNameByEventId(String eventId) throws KException  {
        try {
            for( String dsName : this.metadata.getDataSourceNames() ) {
                TeiidDataSource props = this.metadata.getDataSource(dsName);
                String id = props.getId();
                if( id != null && !StringUtils.isBlank(id) && id.equals(eventId)) {
                    return dsName;
                }
            }
        } catch (AdminException e) {
            throw handleError(e);
        }
        return null;
    }

    public Collection<String> getTeiidDataSourcesNames() throws AdminException {
            return this.metadata.getDataSourceNames();
    }

    private ImageStream createImageStream(OpenShiftClient client, String namespace, String vdbName) {
        ImageStream is = client.imageStreams().inNamespace(namespace).createOrReplaceWithNew()
            .withNewMetadata().withName(vdbName).addToLabels("application", vdbName).endMetadata()
            .done();
        return is;
    }

    private BuildConfig createBuildConfig(OpenShiftClient client, String namespace, String vdbName, ImageStream is,
            PublishConfiguration pc) throws KException {
        String imageStreamName = is.getMetadata().getName()+":latest";
        BuildConfig bc = client.buildConfigs().inNamespace(namespace).createOrReplaceWithNew()
            .withNewMetadata().withName(getBuildConfigName(vdbName))
                .addToLabels("application", vdbName)
                .addToLabels(MANAGED_BY, SYSDESIS)
                .endMetadata()
            .withNewSpec()
                .withRunPolicy("SerialLatestOnly")
                .withNewSource().withType("Binary").endSource()
                .withNewStrategy()
                .withType("Source").withNewSourceStrategy()
                .withNewFrom()
                    .withKind("ImageStreamTag")
                .withName(pc.getBuildImageStream())
                    .withNamespace(namespace)
                .endFrom()
                .withIncremental(false)
                .withEnv(pc.getUserEnvVars())
                .endSourceStrategy()
                .endStrategy()
                .withNewOutput()
                    .withNewTo().withKind("ImageStreamTag").withName(imageStreamName).endTo()
                .endOutput()
                .withNodeSelector(pc.getBuildNodeSelector()).endSpec()
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

        return client.deploymentConfigs().inNamespace(config.namespace()).createOrReplaceWithNew()
            .withNewMetadata().withName(config.vdbName())
                .addToLabels("application", config.vdbName())
            .endMetadata()
            .withNewSpec()
              .withReplicas(1)
              .withNewStrategy().withType("Recreate").endStrategy()
              .addNewTrigger()
                .withType("ConfigChange")
                .withType("ImageChange")
                    .withNewImageChangeParams()
                        .withAutomatic(true)
                        .addToContainerNames(config.vdbName())
                        .withNewFrom().withKind("ImageStreamTag").withName(config.vdbName()+":latest").endFrom()
                    .endImageChangeParams()
              .endTrigger()
              .addToSelector("deploymentConfig", config.vdbName())
              .withNewTemplate()
                .withNewMetadata()
                  .withName(config.vdbName())
                  .addToLabels("application", config.vdbName())
                  .addToLabels("deploymentConfig", config.vdbName())
                  .addToLabels("syndesis.io/type", "datavirtualization")
                  .addToAnnotations("prometheus.io/scrape", "true")
                  .addToAnnotations("prometheus.io/port", "9779")
                .endMetadata()
                .withNewSpec()
                  .addNewContainer()
                    .withName(config.vdbName())
                    .withImage(" ")
                    .withImagePullPolicy("Always")
                    .addAllToEnv(config.publishConfiguration().getEnvironmentVariables())
                    .withNewReadinessProbe()
                      .withNewHttpGet()
                      .withNewPort(8080)
                      .withPath("/actuator/health")
                      .endHttpGet()
                      .withInitialDelaySeconds(30)
                      .withTimeoutSeconds(5)
                      .withPeriodSeconds(20)
                      .withFailureThreshold(5)
                      .withSuccessThreshold(1)
                    .endReadinessProbe()
                    .withNewLivenessProbe()
                      .withNewHttpGet()
                      .withNewPort(8080)
                      .withPath("/actuator/health")
                      .endHttpGet()
                      .withInitialDelaySeconds(30)
                      .withTimeoutSeconds(5)
                      .withPeriodSeconds(20)
                      .withFailureThreshold(5)
                      .withSuccessThreshold(1)
                    .endLivenessProbe()
                    .withNewResources()
                        .addToLimits("memory", new Quantity(config.publishConfiguration().getContainerMemorySize()))
                        .addToLimits("cpu", new Quantity(config.publishConfiguration().getCpuUnits()))
                        // deployment fails with this.
                        // .addToLimits("ephemeral-storage", new Quantity(config.publishConfiguration().getContainerDiskSize()))
                    .endResources()
                    .addAllToPorts(getDeploymentPorts(config.publishConfiguration()))
                  .endContainer()
                .endSpec()
              .endTemplate()
            .endSpec()
            .done();
    }

    private List<ContainerPort> getDeploymentPorts(PublishConfiguration config){
        List<ContainerPort> ports = new ArrayList<>();
        ports.add(createPort(ProtocolType.PROMETHEUS.id(), 9779, "TCP"));
        ports.add(createPort(ProtocolType.JOLOKIA.id(), 8778, "TCP"));
        ports.add(createPort(ProtocolType.JDBC.id(), 31000, "TCP"));
        ports.add(createPort(ProtocolType.PG.id(), 35432, "TCP"));
        if (config.isEnableOData()) {
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

    private Service createService(OpenShiftClient client, String namespace, String vdbName, String type, int srcPort,
            int exposedPort) {
        String serviceName = vdbName+"-"+type;
        debug(vdbName, "Creating the Service of Type " + type + " for VDB "+vdbName);
        Service service = client.services().inNamespace(namespace).withName(serviceName).get();
        if (service == null) {
            client.services().inNamespace(namespace).createNew()
              .withNewMetadata()
                .withName(serviceName)
                .addToLabels("application", vdbName)
                .addToAnnotations(DESCRIPTION_ANNOTATION_LABEL, SERVICE_DESCRIPTION)
              .endMetadata()
              .withNewSpec()
                .withSessionAffinity("ClientIP")
                .addToSelector("application", vdbName)
                .addNewPort()
                  .withName(type)
                  .withPort(exposedPort)
                  .withNewTargetPort()
                    .withStrVal(type)
                  .endTargetPort()
                .endPort()
              .endSpec()
              .done();
            service = client.services().inNamespace(namespace).withName(serviceName).get();
        }
        return service;
    }

    private Service createODataService(OpenShiftClient client, String namespace, String vdbName, String type, int port) {
        String serviceName = vdbName+"-"+type;
        debug(vdbName, "Creating the Service of Type " + type + " for VDB "+vdbName);
        Service service = client.services().inNamespace(namespace).withName(serviceName).get();
        if (service == null) {
            TreeMap<String, String> labels = new TreeMap<String, String>();
            labels.put("application", vdbName);

            TreeMap<String, String> annotations = new TreeMap<String, String>();
            annotations.put(DESCRIPTION_ANNOTATION_LABEL, SERVICE_DESCRIPTION);
            if (this.config.isExposeVia3scale()) {
                labels.put("discovery.3scale.net", "true");
                annotations.put("discovery.3scale.net/scheme", "http");
                annotations.put("discovery.3scale.net/port", Integer.toString(port));
                annotations.put("discovery.3scale.net/description-path", "/openapi.json");
            }

            client.services().inNamespace(namespace).createNew()
              .withNewMetadata()
                .withName(serviceName)
                .addToLabels(labels)
                .addToAnnotations(annotations)
              .endMetadata()
              .withNewSpec()
                .withSessionAffinity("ClientIP")
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
            service = client.services().inNamespace(namespace).withName(serviceName).get();
        }
        return service;
    }

    private String secretName(String name) {
        return name+"-secret";
    }

    private Secret createSecret(OpenShiftClient client, String namespace, String vdbName,
            BuildStatus config) {
        String secretName = secretName(vdbName);

        Secret item = new SecretBuilder().withData(config.publishConfiguration().getSecretVariables()).withNewMetadata()
                .addToLabels("application", vdbName).withName(secretName).endMetadata().build();

        Secret secret = client.secrets().inNamespace(namespace).withName(secretName).createOrReplace(item);

        return secret;
    }

    private Route createRoute(OpenShiftClient client, String namespace, String vdbName, String type) {
        String routeName = vdbName+"-"+type;
        Route route = client.routes().inNamespace(namespace).withName(routeName).get();
        if (route == null) {
            //
            // Create edge termination SSL configuration
            //
            TLSConfigBuilder builder = new TLSConfigBuilder();
            builder.withTermination("edge");

            //
            // Creates secured route
            //
            route = client.routes().inNamespace(namespace).createNew()
              .withNewMetadata()
                .withName(routeName)
                .addToLabels("application", vdbName)
                .addToAnnotations(DESCRIPTION_ANNOTATION_LABEL, SERVICE_DESCRIPTION)
              .endMetadata()
              .withNewSpec()
              .withNewPort().withNewTargetPort().withStrVal(type).endTargetPort().endPort()
              .withNewTo().withName(routeName).endTo()
              .withTls(builder.build())
              .endSpec()
              .done();
        }
        return route;
    }

    private void waitUntilPodIsReady(String vdbName, final OpenShiftClient client, String podName, int nAwaitTimeout) {
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
            error(vdbName, "Publishing - Could not watch pod", e);
        }
    }

    private void createServices(final OpenShiftClient client, final String namespace,
            final String vdbName) {
        createODataService(client, namespace, vdbName, ProtocolType.ODATA.id(), 8080);
        createService(client, namespace, vdbName, ProtocolType.JDBC.id(), 31000, 31000);
        createService(client, namespace, vdbName, ProtocolType.PG.id(), 35432, 5432);
        if (!this.config.isExposeVia3scale()) {
            createRoute(client, namespace, vdbName, ProtocolType.ODATA.id());
        }
        // createRoute(client, namespace, vdbName, RouteType.JDBC.id());
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

    private boolean isDeploymentProgressing(DeploymentConfig dc) {
        DeploymentConfigStatus status = dc.getStatus();
        List<DeploymentCondition> conditions = status.getConditions();
        for (DeploymentCondition cond : conditions) {
            if (cond.getType().equals("Progressing") && cond.getStatus().equals("True")) {
                return true;
            }
        }
        // let's try to deploy five times before giving up.
        if (status.getObservedGeneration() < 4) {
            return true;
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

    private BuildStatus addToQueue(String vdbName, PublishConfiguration publishConfig) {
        BuildStatus work = new BuildStatus(vdbName);
        work.setStatus(Status.SUBMITTED);
        work.setNamespace(ApplicationProperties.getNamespace());
        work.setStatusMessage("Submitted build for configuration");
        work.setLastUpdated();
        work.setPublishConfiguration(publishConfig);

        this.workExecutor.submit(new BuildStatusRunner(work));
        return work;
    }

    protected void configureBuild(BuildStatus work) {
        work.setStatus(Status.CONFIGURING);
        configureService.execute(new Runnable() {
            @Override
            public void run() {
                info(work.vdbName(), "Publishing  - Configuring ...");

                final OpenShiftClient client = openshiftClient();
                String namespace = work.namespace();
                PublishConfiguration publishConfig = work.publishConfiguration();
                VDBMetaData vdb = publishConfig.getVDB();
                OAuthCredentials oauthCreds = publishConfig.getOAuthCredentials();

                String vdbName = work.vdbName();
                try {
                    info(vdbName, "Publishing - Checking for base image");

                    // create build contents as tar file
                    info(vdbName, "Publishing - Creating zip archive");
                    GenericArchive archive = ShrinkWrap.create(GenericArchive.class, "contents.tar");
                    String pomFile = generatePomXml(oauthCreds, vdb, publishConfig.isEnableOData());

                    debug(vdbName, "Publishing - Generated pom file: " + NEW_LINE + pomFile);
                    archive.add(new StringAsset(pomFile), "pom.xml");

                    normalizeDataSourceNames(vdb);

                    AccessibleByteArrayOutputStream vdbContents = DefaultMetadataInstance.toBytes(vdb);
                    archive.add(new ByteArrayAsset(new ByteArrayInputStream(vdbContents.getBuffer(), 0, vdbContents.getCount())), "/src/main/resources/" + vdbName + "-vdb.xml");

                    InputStream configIs = this.getClass().getClassLoader().getResourceAsStream("s2i/application.properties");
                    archive.add(new ByteArrayAsset(ObjectConverterUtil.convertToByteArray(configIs)),
                                "/src/main/resources/application.properties");

                    InputStream dsIs = buildDataSourceBuilders(vdb);
                    archive.add(new ByteArrayAsset(ObjectConverterUtil.convertToByteArray(dsIs)),
                            "/src/main/java/io/integration/DataSources.java");

                    InputStream appIs = this.getClass().getClassLoader().getResourceAsStream("s2i/Application.java");
                    archive.add(new ByteArrayAsset(ObjectConverterUtil.convertToByteArray(appIs)),
                                "/src/main/java/io/integration/Application.java");

                    info(vdbName, "Publishing - Converting archive to TarExport");
                    InputStream buildContents = archive.as(TarExporter.class).exportAsInputStream();
                    info(vdbName, "Publishing - Completed creating build contents construction");

                    info(vdbName, "Publishing - Creating image stream");
                    // use the contents to invoke a binary build
                    ImageStream is = createImageStream(client, namespace, vdbName);

                    info(vdbName, "Publishing - Creating build config");
                    BuildConfig buildConfig = createBuildConfig(client, namespace, vdbName, is, publishConfig);

                    info(vdbName, "Publishing - Creating build");
                    Build build = createBuild(client, namespace, buildConfig, buildContents);

                    String buildName = build.getMetadata().getName();
                    info(vdbName, "Publishing - Build created: " + buildName);

                    PodOperationsImpl publishPod = (PodOperationsImpl)client.pods().withName(buildName + "-build");

                    info(vdbName, "Publishing - Awaiting pod readiness ...");
                    waitUntilPodIsReady(vdbName, client, buildName + "-build", 20);

                    info(vdbName, "Publishing - Fetching environment variables for vdb data sources");

                    publishConfig.addEnvironmentVariables(
                            getEnvironmentVariablesForVDBDataSources(oauthCreds, vdb, publishConfig));

                    publishConfig.addSecretVariables(
                            getSecretVariablesForVDBDataSources(oauthCreds, vdb, publishConfig));

                    work.setBuildName(buildName);
                    work.setStatusMessage("Build Running");
                    work.setPublishPodName(publishPod.getName());
                    work.setLastUpdated();
                    work.setStatus(Status.BUILDING);

                    info(vdbName, "Publishing  - Configuration completed. Building ... Pod Name: " + work.publishPodName());

                } catch (Exception ex) {
                    work.setStatus(Status.FAILED);
                    work.setStatusMessage(ex.getLocalizedMessage());
                    error(work.vdbName(), "Publishing - Build failed", ex);
                } finally {
                    if (client != null) {
                        client.close();
                    }
                    //
                    // Building is a long running operation so close the log file
                    //
                    closeLog(vdbName);
                }
            }
        });
    }

    protected void normalizeDataSourceNames(VDBMetaData vdb) throws KException {
        for (ModelMetaData model : vdb.getModelMetaDatas().values()) {
            for (SourceMappingMetadata source : model.getSources().values()) {
                String name = source.getName().toLowerCase();
                name = name.replace("-", "");
                source.setConnectionJndiName(name);
            }
        }
    }

    private static final String DS_TEMPLATE =
            "    @ConfigurationProperties(prefix = \"spring.datasource.{name}\")\n" +
            "    @Bean(\"{name}\")\n" +
            "    public DataSource {method-name}() {\n" +
            "        return DataSourceBuilder.create().build();\n" +
            "    }";

    protected InputStream buildDataSourceBuilders(VDBMetaData vdb) throws KException {
        StringWriter sw = new StringWriter();
        sw.write("package io.integration;\n" +
                "\n" +
                "import javax.sql.DataSource;\n" +
                "\n" +
                "import org.springframework.boot.jdbc.DataSourceBuilder;\n" +
                "import org.springframework.boot.context.properties.ConfigurationProperties;\n" +
                "import org.springframework.context.annotation.Bean;\n" +
                "import org.springframework.context.annotation.Configuration;\n" +
                "\n" +
                "@Configuration\n" +
                "public class DataSources {\n");

        for (Model model : vdb.getModels()) {
            for (String name : model.getSourceNames()) {
                String replacement = model.getSourceConnectionJndiName(name);
                sw.write(DS_TEMPLATE.replace("{name}", replacement).replace("{method-name}", replacement));
                sw.write("\n");
            }
        }
        sw.write("}\n");

        try {
            return new ByteArrayInputStream(sw.toString().getBytes("UTF-8"));
        } catch (UnsupportedEncodingException e) {
            throw new KException(e);
        }
    }

    /**
     * Publish the vdb as a virtualization
     *
     * @return the build status of the virtualization
     * @throws KException if error occurs
     */
    public BuildStatus publishVirtualization(PublishConfiguration publishConfig, String vdbName) throws KException {
        removeLog(vdbName);
        info(vdbName, "Publishing - Start publishing of virtualization: " + vdbName);

        BuildStatus status = getVirtualizationStatus(vdbName);
        info(vdbName, "Publishing - Virtualization status: " + status.status());

        if (status.status().equals(Status.BUILDING)) {
            info(vdbName, "Publishing - Previous build request in progress, failed to submit new build request: "
                    + status.status());
            return status;
        } else {
            info(vdbName, "Publishing - Adding to work queue for build");
            status = addToQueue(vdbName, publishConfig);

            debug(vdbName, "Publishing - Initiating work monitor if not already running");

            info(vdbName, "Publishing - Status of build + " + status.status());
            return status;
        }
    }

    Map<String, String> getSecretVariablesForVDBDataSources(OAuthCredentials oauthCreds, VDBMetaData vdb,
            PublishConfiguration publishConfig) throws KException {
        Map<String, String> properties = new HashMap<>();
        for (Model model : vdb.getModels()) {
            for (String source : model.getSourceNames()) {
                DefaultSyndesisDataSource ds = getSyndesisDataSource(oauthCreds, source);
                if (ds == null) {
                    throw new KException("Datasource "+source+" not found in Syndesis");
                }

                // if null this is either file, ws, kind of source where service catalog source does not exist
                DataSourceDefinition def = ds.getDefinition();
                if (def == null) {
                    throw new KException("Failed to determine the source type for "
                            + source + " in VDB " + vdb.getName());
                }

                Map<String, String> config = def.getPublishedImageDataSourceProperties(ds);
                if (config != null) {
                    for (Map.Entry<String, String> entry : config.entrySet())
                        properties.put(entry.getKey(), Base64.getEncoder()
                                .encodeToString(encryptionComponent.decrypt(entry.getValue()).getBytes()));
                }
            }
        }
        return properties;
    }

    Collection<EnvVar> getEnvironmentVariablesForVDBDataSources(OAuthCredentials oauthCreds, VDBMetaData vdb,
            PublishConfiguration publishConfig) throws KException {
        List<EnvVar> envs = new ArrayList<>();
        for (Model model : vdb.getModels()) {
            for (String source : model.getSourceNames()) {
                DefaultSyndesisDataSource ds = getSyndesisDataSource(oauthCreds, source);
                if (ds == null) {
                    throw new KException("Datasource "+source+" not found in Syndesis");
                }

                // if null this is either file, ws, kind of source where service catalog source does not exist
                DataSourceDefinition def = ds.getDefinition();
                if (def == null) {
                    throw new KException("Failed to determine the source type for "
                            + source + " in VDB " + vdb.getName());
                }
                // data source properties as ENV variables
                def.getPublishedImageDataSourceProperties(ds).forEach((K,V) -> {
                    envs.add(envFromSecret(secretName(vdb.getName()), (String)K));
                });
            }
        }
        // These options need to be removed after the base image gets updated with them natively
        for (Map.Entry<String, String> entry : publishConfig.getUserEnvironmentVariables().entrySet()) {
            envs.add(env(entry.getKey(), entry.getValue()));
        }
        envs.add(env("VDB_FILE", vdb.getName()+"-vdb.xml"));
        envs.add(env("JAVA_OPTIONS", publishConfig.getUserJavaOptions()));
        return envs;
    }

    protected String envName(String key) {
        key = key.replace(StringConstants.HYPHEN, "");
        key = key.replace(StringConstants.DOT, StringConstants.UNDERSCORE);
        return key.toUpperCase();
    }

    protected EnvVar env(String name, String value) {
        return new EnvVarBuilder().withName(name).withValue(value).build();
    }

    protected EnvVar envFromSecret(String secret, String key) {
        return new EnvVarBuilder().withName(envName(key))
                .withValueFrom(new EnvVarSourceBuilder().withNewSecretKeyRef(key, secret, false).build()).build();
    }

    public BuildStatus getVirtualizationStatus(String vdbName) {
        BuildStatus status = getVirtualizationStatusFromQueue(vdbName);
        if (status != null) {
            return status;
        }
        OpenShiftClient client = openshiftClient();
        try {
            return getVDBService(vdbName, ApplicationProperties.getNamespace(), client);
        } finally {
            client.close();
        }
    }

    public String getVirtualizationLog(String vdbName) {
        String logPath = getLogPath(vdbName);
        File logFile = new File(logPath);
        if (! logFile.exists())
            return "No log available";

        try {
            return ObjectConverterUtil.convertFileToString(logFile);
        } catch (IOException e) {
            return "No log available";
        }
    }

    private BuildStatus getVDBService(String vdbName, String namespace, final OpenShiftClient client) {
        BuildStatus status = new BuildStatus(vdbName);
        status.setNamespace(namespace);

        BuildList buildList = client.builds().inNamespace(namespace).withLabel("application", vdbName).list();
        if ((buildList !=null) && !buildList.getItems().isEmpty()) {
            Build build = buildList.getItems().get(0);
            status.setBuildName(build.getMetadata().getName());
            if (Builds.isCancelled(build.getStatus().getPhase())) {
                status.setStatus(Status.CANCELLED);
                status.setStatusMessage(build.getStatus().getMessage());
            } else if (Builds.isFailed(build.getStatus().getPhase())) {
                status.setStatus(Status.FAILED);
                status.setStatusMessage(build.getStatus().getMessage());
            } else if (Builds.isCompleted(build.getStatus().getPhase())) {
                DeploymentConfig dc = client.deploymentConfigs().inNamespace(namespace).withName(vdbName).get();
                if (dc != null) {
                    status.setStatus(Status.DEPLOYING);
                    status.setDeploymentName(dc.getMetadata().getName());
                    if (isDeploymentInReadyState(dc)) {
                        status.setStatus(Status.RUNNING);

                        //
                        // Only if status is running then populate the routes
                        // for this virtualization
                        //
                        ProtocolType[] types = { ProtocolType.ODATA, ProtocolType.JDBC, ProtocolType.PG };
                        for (ProtocolType type : types) {
                            try {
                                RouteStatus route = getRoute(vdbName, type);
                                if (route == null) {
                                    continue;
                                }
                                status.addRoute(route);
                            } catch(KubernetesClientException e) {
                                // ignore..
                            }
                        }
                    }

                    if (!isDeploymentProgressing(dc)) {
                        status.setStatus(Status.FAILED);
                    }

                    DeploymentCondition cond = getDeploymentConfigStatus(dc);
                    if (cond != null) {
                        status.setStatusMessage(cond.getMessage());
                    } else {
                        status.setStatusMessage("Available condition not found in deployment, delete the service and re-deploy?");
                    }
                } else {
                    status.setStatusMessage("Build Completed, but no deployment found. Reason unknown, please redeploy");
                    status.setStatus(Status.FAILED);
                }
            } else {
                status.setStatus(Status.BUILDING);
                status.setStatusMessage(build.getStatus().getMessage());
            }
        } else {
            // special case when there is dangling replication controller after delete is found
            List<ReplicationController> rcs = client.replicationControllers().inNamespace(namespace)
                    .withLabel("application", vdbName).list().getItems();
            if (!rcs.isEmpty()) {
                ReplicationController rc = rcs.get(0);
                if (rc.getStatus().getReplicas() == 0) {
                    status.setStatusMessage("Build Completed, but no deployment found. Reason unknown, please redeploy");
                    status.setStatus(Status.FAILED);
                }
            }
        }
        status.setLastUpdated();
        return status;
    }

    public Collection<BuildStatus> getVirtualizations(boolean includeInQueue){
        String namespace = ApplicationProperties.getNamespace();
        final OpenShiftClient client = openshiftClient();
        Map<String, BuildStatus> services = new HashMap<>();
        try {
            BuildList bl = client.builds().inNamespace(namespace).withLabel(MANAGED_BY, SYSDESIS).list();
            for (Build b : bl.getItems()) {
                String vdbName = b.getMetadata().getLabels().get("application");
                services.put(vdbName, getVDBService(vdbName, namespace, client));
            }
        } finally {
            client.close();
        }

        if (includeInQueue) {
            for(Runnable r : workExecutor.getQueue()) {
                if (r instanceof BuildStatusRunner) {
                    BuildStatusRunner runner = (BuildStatusRunner)r;
                    services.put(runner.work.vdbName(), runner.work);
                }
            }
        }

        if (logger.isDebugEnabled() ) {
            for (BuildStatus build : services.values()) {
                debug(build.vdbName(), "Publish Status: " + build.status());
            }
        }
        return services.values();
    }

    public BuildStatus deleteVirtualization(String vdbName) {
        BuildStatus runningBuild = getVirtualizationStatusFromQueue(vdbName);

        boolean queue = false;
        if (runningBuild == null) {
            runningBuild = getVirtualizationStatus(vdbName);
            queue = true;
        }

        if (BuildStatus.Status.NOTFOUND.equals(runningBuild.status())) {
            return runningBuild;
        }

        info(vdbName, "Deleting virtualization deployed as Service");
        final String inProgressBuildName = runningBuild.buildName();
        final BuildStatus status = runningBuild;
        configureService.submit(new Callable<Boolean>() {
            @Override
            public Boolean call() throws Exception {
                deleteVDBServiceResources(vdbName, inProgressBuildName, status);
                debug(vdbName, "finished deleteing " + vdbName + " service");
                return true;
            }
        });
        runningBuild.setStatus(Status.DELETE_SUBMITTED);
        runningBuild.setStatusMessage("delete submitted");
        // since delete is async process too, monitor it in the monitor thread.
        if (queue) {
            workExecutor.submit(new BuildStatusRunner(runningBuild));
        }

        return runningBuild;
    }

    private BuildStatus getVirtualizationStatusFromQueue(String vdbName) {
        for(Runnable r : workExecutor.getQueue()) {
            if (r instanceof BuildStatusRunner) {
                BuildStatusRunner status = (BuildStatusRunner)r;
                if (status.work.vdbName().equals(vdbName)) {
                    return status.work;
                }
            }
        }
        return null;
    }

    private void deleteVDBServiceResources(String vdbName, String inProgressBuildName, BuildStatus status) {
        final OpenShiftClient client = openshiftClient();
        final String namespace = ApplicationProperties.getNamespace();

        try {
            // delete routes first
            client.routes().inNamespace(namespace).withName(vdbName + HYPHEN + ProtocolType.ODATA.id()).delete();
            // delete services next
            client.services().inNamespace(namespace).withName(vdbName + HYPHEN + ProtocolType.JDBC.id()).delete();
            client.services().inNamespace(namespace).withName(vdbName + HYPHEN + ProtocolType.ODATA.id()).delete();
            client.services().inNamespace(namespace).withName(vdbName + HYPHEN + ProtocolType.PG.id()).delete();
        } catch (KubernetesClientException e ) {
            error(vdbName, e.getMessage());
            error(vdbName, "requeueing the delete request");
            status.setStatus(Status.DELETE_REQUEUE);
        }

        try {
            // delete builds
            client.builds().inNamespace(namespace).withLabel("application", vdbName).delete();
        } catch (KubernetesClientException e ) {
            error(vdbName, e.getMessage());
            error(vdbName, "requeueing the delete request");
            status.setStatus(Status.DELETE_REQUEUE);
        }
        try {
            // delete pods
            client.pods().inNamespace(namespace).withLabel("application", vdbName).delete();
        } catch (KubernetesClientException e ) {
            error(vdbName, e.getMessage());
            error(vdbName, "requeueing the delete request");
            status.setStatus(Status.DELETE_REQUEUE);
        }
        try {
            // delete image streams
            client.imageStreams().inNamespace(namespace).withLabel("application", vdbName).delete();
        } catch (KubernetesClientException e ) {
            error(vdbName, e.getMessage());
            error(vdbName, "requeueing the delete request");
            status.setStatus(Status.DELETE_REQUEUE);
        }
        try {
            // delete replication controller
            client.replicationControllers().inNamespace(namespace).withLabel("application", vdbName).delete();
        } catch (KubernetesClientException e ) {
            error(vdbName, e.getMessage());
            error(vdbName, "requeueing the delete request");
            status.setStatus(Status.DELETE_REQUEUE);
        }
        try {
            // deployment configs
            client.deploymentConfigs().inNamespace(namespace).withName(vdbName).delete();
        } catch (KubernetesClientException e ) {
            error(vdbName, e.getMessage());
            error(vdbName, "requeueing the delete request");
            status.setStatus(Status.DELETE_REQUEUE);
        }
        try {
            // secrets
            client.secrets().inNamespace(namespace).withName(secretName(vdbName)).delete();
        } catch (KubernetesClientException e ) {
            error(vdbName, e.getMessage());
            error(vdbName, "requeueing the delete request");
            status.setStatus(Status.DELETE_REQUEUE);
        }
        try {
            // delete build configuration
            client.buildConfigs().inNamespace(namespace).withLabel("application", vdbName).delete();
        } catch (KubernetesClientException e ) {
            error(vdbName, e.getMessage());
            error(vdbName, "requeueing the delete request");
            status.setStatus(Status.DELETE_REQUEUE);
        }
        try {
            // checking 2nd time as, I found this not being deleted completely
            // delete replication controller
            client.replicationControllers().inNamespace(namespace).withLabel("application", vdbName).delete();
        } catch (KubernetesClientException e ) {
            error(vdbName, e.getMessage());
            error(vdbName, "requeueing the delete request");
            status.setStatus(Status.DELETE_REQUEUE);
        }

        try {
            // delete image streams
            client.imageStreams().inNamespace(namespace).withLabel("application", vdbName).delete();
        } catch (KubernetesClientException e ) {
            error(vdbName, e.getMessage());
            error(vdbName, "requeueing the delete request");
            status.setStatus(Status.DELETE_REQUEUE);
        }

        status.setStatus(Status.DELETE_DONE);
    }

    private RouteStatus getRoute(String vdbName, ProtocolType protocolType) {
        String namespace = ApplicationProperties.getNamespace();
        final OpenShiftClient client = openshiftClient();
        try {
            RouteStatus theRoute = null;
            debug(vdbName, "Getting route of type " + protocolType.id() + " for Service");
            RouteList routes = client.routes().inNamespace(namespace).list();
            if (routes == null || routes.getItems().isEmpty())
                return theRoute;

            for (Route route : routes.getItems()) {
                ObjectMeta metadata = route.getMetadata();
                String name = metadata.getName();
                if (! name.endsWith(HYPHEN + protocolType.id()))
                    continue;

                RouteSpec spec = route.getSpec();
                String target = spec.getTo().getName();

                Map<String, String> annotations = metadata.getAnnotations();
                String description = annotations.get(DESCRIPTION_ANNOTATION_LABEL);
                if (description == null || ! SERVICE_DESCRIPTION.equals(description))
                    continue;

                //
                // Check we have the right route for the vdb in question
                //
                if (! target.equals(vdbName + HYPHEN + protocolType.id()))
                    continue;

                theRoute = new RouteStatus(name, protocolType);
                theRoute.setHost(spec.getHost());
                theRoute.setPath(spec.getPath());
                theRoute.setPort(spec.getPort().getTargetPort().getStrVal());
                theRoute.setTarget(target);
                theRoute.setSecure(spec.getTls() != null);
            }

            return theRoute;

        } finally {
            client.close();
        }
    }

    /**
     * This method generates the pom.xml file, that needs to be saved in the root of the project.
     * @param oauthCreds - token for Openshift authentication
     * @param vdb - VDB for which pom.xml is generated
     * @return pom.xml contents
     * @throws KException
     */
    protected String generatePomXml(OAuthCredentials oauthCreds, VDBMetaData vdb, boolean enableOdata) throws KException {
        try {
            StringBuilder builder = new StringBuilder();
            InputStream is = this.getClass().getClassLoader().getResourceAsStream("s2i/template-pom.xml");
            builder.append(new String(ObjectConverterUtil.convertToByteArray(is)));

            StringBuilder vdbSourceNames = new StringBuilder();
            StringBuilder vdbDependencies = new StringBuilder();

            String vdbName = vdb.getName();
            List<Model> models = vdb.getModels();
            for (Model model : models) {
                for (String source : model.getSourceNames()) {
                    DefaultSyndesisDataSource ds = getSyndesisDataSource(oauthCreds, source);
                    if (ds == null) {
                        throw new KException("Datasource " + source + " not found");
                    }
                    DataSourceDefinition def = ds.getDefinition();
                    if (def == null) {
                        throw new KException("Failed to determine the source type for "
                                + source + " in VDB " + vdb.getName());
                    }

                    vdbSourceNames.append(source).append(StringConstants.SPACE); // this used as label
                    vdbDependencies.append(def.getPomDendencies());
                    vdbDependencies.append(StringConstants.NEW_LINE);
                }
            }

            if (enableOdata) {
                vdbDependencies.append(StringConstants.NEW_LINE).append("<dependency>"
                        + "<groupId>org.teiid</groupId>"
                        + "<artifactId>spring-odata</artifactId>"
                        + "<version>${version.springboot.teiid}</version>"
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
