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
package org.komodo.rest.service;

import static org.komodo.rest.datavirtualization.RelationalMessages.Error.*;

import java.io.InputStream;
import java.util.ArrayList;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.TreeSet;
import java.util.regex.Pattern;
import java.util.zip.ZipEntry;
import java.util.zip.ZipInputStream;
import java.util.zip.ZipOutputStream;

import org.komodo.KException;
import org.komodo.StringConstants;
import org.komodo.WorkspaceManager;
import org.komodo.datavirtualization.DataVirtualization;
import org.komodo.datavirtualization.SourceSchema;
import org.komodo.datavirtualization.ViewDefinition;
import org.komodo.metadata.TeiidDataSource;
import org.komodo.openshift.BuildStatus;
import org.komodo.openshift.BuildStatus.RouteStatus;
import org.komodo.openshift.ProtocolType;
import org.komodo.openshift.TeiidOpenShiftClient;
import org.komodo.rest.KomodoService;
import org.komodo.rest.V1Constants;
import org.komodo.rest.datavirtualization.ImportPayload;
import org.komodo.rest.datavirtualization.KomodoStatusObject;
import org.komodo.rest.datavirtualization.RelationalMessages;
import org.komodo.rest.datavirtualization.RestDataVirtualization;
import org.komodo.rest.datavirtualization.v1.DataVirtualizationV1Adapter;
import org.komodo.rest.datavirtualization.v1.SourceV1;
import org.komodo.rest.datavirtualization.v1.ViewDefinitionV1Adapter;
import org.komodo.utils.PathUtils;
import org.komodo.utils.StringNameValidator;
import org.komodo.utils.StringUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.dao.DataIntegrityViolationException;
import org.springframework.http.HttpHeaders;
import org.springframework.http.HttpStatus;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;
import org.springframework.util.MultiValueMap;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RestController;
import org.springframework.web.multipart.MultipartFile;
import org.springframework.web.server.ResponseStatusException;
import org.springframework.web.servlet.mvc.method.annotation.StreamingResponseBody;
import org.teiid.adminapi.impl.ModelMetaData;
import org.teiid.metadata.Schema;
import org.teiid.metadata.Table;
import org.teiid.util.FullyQualifiedName;

import com.fasterxml.jackson.core.JsonFactory;
import com.fasterxml.jackson.core.JsonGenerator;
import com.fasterxml.jackson.databind.DeserializationFeature;
import com.fasterxml.jackson.databind.ObjectMapper;

import io.swagger.annotations.Api;
import io.swagger.annotations.ApiOperation;
import io.swagger.annotations.ApiParam;
import io.swagger.annotations.ApiResponse;
import io.swagger.annotations.ApiResponses;

/**
 * A Komodo REST service for obtaining Dataservice information from the workspace.
 */
@RestController
@RequestMapping(value = V1Constants.APP_PATH + V1Constants.FS + V1Constants.WORKSPACE_SEGMENT
        + StringConstants.FS + V1Constants.DATA_SERVICES_SEGMENT)
@Api(tags = { V1Constants.DATA_SERVICES_SEGMENT })
public final class KomodoDataserviceService extends KomodoService {

    /**
     * To be a valid schema name we don't allow .
     * Since we'll add the dv- prefix, we don't char what it starts with,
     * but we're still required to end with a letter/number
     */
    Pattern DATAVIRTUALIZATION_PATTERN = Pattern.compile("[-a-z0-9]*[a-z0-9]", Pattern.CASE_INSENSITIVE); //$NON-NLS-1$

    private static final StringNameValidator VALIDATOR = new StringNameValidator();

    @Autowired
    private TeiidOpenShiftClient openshiftClient;

    @Autowired
    private KomodoMetadataService metadataService;

    @Autowired
    private KomodoUtilService utilService;

    /**
     * Get the Dataservices from the komodo repository
     * @return a JSON document representing all the Dataservices in the Komodo workspace (never <code>null</code>)
     * @throws Exception
     */
    @RequestMapping(method = RequestMethod.GET, produces= { MediaType.APPLICATION_JSON_VALUE })
    @ApiOperation(value = "Return the collection of data services",
        response = RestDataVirtualization.class, responseContainer = "List")
    @ApiResponses(value = { @ApiResponse(code = 403, message = "An error has occurred.") })
    public List<RestDataVirtualization> getDataservices() throws Exception {

        Iterable<? extends DataVirtualization> dataServices = kengine.runInTransaction(true, ()->{
            return getWorkspaceManager().findDataVirtualizations();
        });

        final List<RestDataVirtualization> entities = new ArrayList<>();
        for (final DataVirtualization dataService : dataServices) {
            RestDataVirtualization entity = createRestDataservice(dataService);
            entities.add(entity);
            LOGGER.debug("getDataservices:Dataservice '%s' entity was constructed", dataService.getName());
        }
        return entities;
    }

    private RestDataVirtualization createRestDataservice(final DataVirtualization dataService) throws KException {
        RestDataVirtualization entity = new RestDataVirtualization(dataService);
        entity.setServiceViewModel(dataService.getName());
        // Set published status of dataservice
        BuildStatus status = this.openshiftClient.getVirtualizationStatus(dataService.getName());
        entity.setPublishedState(status.status().name());
        entity.setPublishPodName(status.publishPodName());
        entity.setPodNamespace(status.namespace());
        entity.setOdataHostName(getOdataHost(status));
        entity.setEmpty(this.getWorkspaceManager().findViewDefinitionsNames(dataService.getName()).isEmpty());
        entity.setUsedBy(status.getUsedBy());
        return entity;
    }

    /**
     * @param dataserviceName the id of the Dataservice being retrieved (cannot be empty)
     * @return the JSON representation of the Dataservice (never <code>null</code>)
     * @throws Exception
     */
    @RequestMapping(value = V1Constants.DATA_SERVICE_PLACEHOLDER, method = RequestMethod.GET, produces = {
            MediaType.APPLICATION_JSON_VALUE })
    @ApiOperation(value = "Find dataservice by name", response = RestDataVirtualization.class)
    @ApiResponses(value = { @ApiResponse(code = 404, message = "No Dataservice could be found with name"),
            @ApiResponse(code = 406, message = "Only JSON is returned by this operation"),
            @ApiResponse(code = 403, message = "An error has occurred.") })
    public RestDataVirtualization getDataservice(
            @ApiParam(value = "name of the dataservice to be fetched",
            required = true) final @PathVariable("dataserviceName") String dataserviceName)
            throws Exception {

        DataVirtualization dv = kengine.runInTransaction(true, () -> {
             return getWorkspaceManager().findDataVirtualization(dataserviceName);
        });

        if (dv == null) {
            throw notFound( dataserviceName );
        }

        RestDataVirtualization dataservice = createRestDataservice(dv);
        LOGGER.debug("getDataservice:Dataservice '{0}' entity was constructed", dataservice.getName()); //$NON-NLS-1$
        return dataservice;
    }

    /**
     * Create a new DataService in the komodo repository
     *
     * @param dataserviceName the dataservice name (cannot be empty)
     * @return a JSON representation of the new dataservice (never <code>null</code>)
     * @throws Exception
     */
    @RequestMapping(value = FS + V1Constants.DATA_SERVICE_PLACEHOLDER,
            method = RequestMethod.POST,
            produces= { MediaType.APPLICATION_JSON_VALUE },
            consumes = { MediaType.APPLICATION_JSON_VALUE })
    @ApiOperation(value = "Create a dataservice in the workspace")
    @ApiResponses(value = { @ApiResponse(code = 406, message = "Only JSON is returned by this operation"),
            @ApiResponse(code = 403, message = "An error has occurred.") })
    public ResponseEntity<String> createDataservice(
            @ApiParam(value = "Name of the data service", required = true) final @PathVariable("dataserviceName") String dataserviceName,
            @ApiParam(required = true) @RequestBody final RestDataVirtualization restDataservice) throws Exception {

        final String jsonDataserviceName = restDataservice.getName();
        // Error if the name is missing from the supplied json body
        if (StringUtils.isBlank(jsonDataserviceName)) {
            throw forbidden(RelationalMessages.Error.DATASERVICE_SERVICE_MISSING_NAME);
        }

        // Error if the name parameter is different than JSON name
        final boolean namesMatch = dataserviceName.equals(jsonDataserviceName);
        if (!namesMatch) {
            throw forbidden(DATASERVICE_SERVICE_SERVICE_NAME_ERROR, dataserviceName, jsonDataserviceName);
        }

        String message = getValidationMessage(dataserviceName);
        if (message != null) {
            throw new ResponseStatusException(HttpStatus.FORBIDDEN, message);
        }

        // create new Dataservice
        try {
            return kengine.runInTransaction(false, () -> {
                final DataVirtualization dataservice = getWorkspaceManager().createDataVirtualization(dataserviceName);
                dataservice.setDescription(restDataservice.getDescription());
                return ResponseEntity.ok(dataserviceName + " Successfully created");
            });
        } catch (DataIntegrityViolationException e) {
            throw error(HttpStatus.CONFLICT, RelationalMessages.Error.DATASERVICE_SERVICE_CREATE_ALREADY_EXISTS);
        }
    }

    /**
     * Delete the specified Dataservice from the komodo repository
     *
     * @param dataserviceName the name of the data service to remove (cannot be <code>null</code>)
     * @return a JSON document representing the results of the removal
     * @throws Exception
     */
    @RequestMapping(value = V1Constants.DATA_SERVICE_PLACEHOLDER, method = RequestMethod.DELETE, produces = {
            MediaType.APPLICATION_JSON_VALUE })
    @ApiOperation(value = "Delete a dataservice from the workspace")
    @ApiResponses(value = { @ApiResponse(code = 406, message = "Only JSON is returned by this operation"),
            @ApiResponse(code = 403, message = "An error has occurred.") })
    public KomodoStatusObject deleteDataservice(@ApiParam(value = "Name of the data service to be deleted", required = true) final @PathVariable("dataserviceName") String dataserviceName) throws Exception {

        KomodoStatusObject kso = kengine.runInTransaction(false, ()->{
            final WorkspaceManager wkspMgr = getWorkspaceManager();

            // Delete the Dataservice. The view definitions will cascade
            if (!wkspMgr.deleteDataVirtualization(dataserviceName)) {
                throw notFound(dataserviceName);
            }

            KomodoStatusObject status = new KomodoStatusObject("Delete Status"); //$NON-NLS-1$
            status.addAttribute(dataserviceName, "Successfully deleted"); //$NON-NLS-1$
            return status;
        });

        //deleted/txn committed, update runtime
        //there is a small chance that a dv with the same name was recreated in the meantime,
        //but since this vdb is created on-demand we're good
        try {
            metadataService.removeVdb(DataVirtualization.getPreviewVdbName(dataserviceName));
        } catch (KException e) {
            LOGGER.debug("error removing preview vdb", e); //$NON-NLS-1$
        }
        return kso;
    }

    /**
     * @param dataserviceName the data service name being validated (cannot be empty)
     * @return the response (never <code>null</code>) with an entity that is either
     *         an empty string, when the name is valid, or an error message
     * @throws Exception
     */
    @RequestMapping(value = V1Constants.NAME_VALIDATION_SEGMENT + FS
            + V1Constants.DATA_SERVICE_PLACEHOLDER, method = RequestMethod.GET, produces = { "text/plain" })
    @ApiOperation(value = "Returns an error message if the data service name is invalid")
    @ApiResponses(value = {
            @ApiResponse(code = 400, message = "The URI cannot contain encoded slashes or backslashes."),
            @ApiResponse(code = 403, message = "An unexpected error has occurred."),
            @ApiResponse(code = 500, message = "The dataservice name cannot be empty.") })
    public ResponseEntity<String> validateDataserviceName(@ApiParam(value = "The dataservice name being checked", required = true) final @PathVariable("dataserviceName") String dataserviceName) throws Exception {

        String validationMessage = getValidationMessage(dataserviceName);
        if (validationMessage != null) {
            return ResponseEntity.ok().body(validationMessage);
        }

        // check for duplicate name
        final boolean inUse = kengine.runInTransaction(true, () -> {
            //from the pattern validation, there's no escaping necessary
            return getWorkspaceManager().isNameInUse(dataserviceName);
        });

        // name is a duplicate
        if (inUse) {
            return ResponseEntity.ok().body(RelationalMessages.getString(DATASERVICE_SERVICE_NAME_EXISTS));
        }

        return ResponseEntity.ok().build();
    }

    private String getValidationMessage(final String dataserviceName) {
        final String errorMsg = VALIDATOR.checkValidName(dataserviceName);

        if (errorMsg != null) {
            return errorMsg;
        }

        if (!DATAVIRTUALIZATION_PATTERN.matcher(dataserviceName).matches()) {
            return "Must match pattern " + DATAVIRTUALIZATION_PATTERN.pattern(); //$NON-NLS-1$
        }

        TreeSet<String> taken = new TreeSet<>(String.CASE_INSENSITIVE_ORDER);
        taken.addAll(ModelMetaData.getReservedNames());

        if (taken.contains(dataserviceName)) {
            return dataserviceName + " is a reserved name."; //$NON-NLS-1$
        }

        return null;
    }

    @RequestMapping(value = StringConstants.FS + V1Constants.DATA_SERVICE_PLACEHOLDER +
            StringConstants.FS + V1Constants.IMPORT + StringConstants.FS
            + V1Constants.KOMODO_SOURCE_PLACEHOLDER, method = RequestMethod.PUT, produces = { MediaType.APPLICATION_JSON_VALUE })
    @ApiOperation(value = "Import views from a given source", response = KomodoStatusObject.class)
    @ApiResponses(value = { @ApiResponse(code = 406, message = "Only JSON is returned by this operation"),
            @ApiResponse(code = 403, message = "An error has occurred.") })
    public KomodoStatusObject importViews(@ApiParam(value = "Name of the dataservice", required = true)
            final @PathVariable("dataserviceName")
            String dataserviceName,

            @ApiParam( value = "Name of the komodo source", required = true )
            final @PathVariable( "komodoSourceName" )
            String komodoSourceName,

            @ApiParam(value = "Import Payload", required = true)
            @RequestBody
            final ImportPayload importPayload) throws Exception {

        KomodoStatusObject kso = kengine.runInTransaction(false, () -> {
            DataVirtualization dataservice = getWorkspaceManager().findDataVirtualization(dataserviceName);
            if (dataservice == null) {
                throw notFound( dataserviceName );
            }

            Schema s = metadataService.findSchema(komodoSourceName);

            if (s == null) {
                throw notFound( komodoSourceName );
            }

            ServiceVdbGenerator serviceVdbGenerator = new ServiceVdbGenerator(metadataService);

            KomodoStatusObject result = new KomodoStatusObject("Import Status"); //$NON-NLS-1$

            List<ViewDefinition> toSave = new ArrayList<>();
            for (String name : importPayload.getTables()) {
                Table t = s.getTable(name);
                if (t == null) {
                    //could be an error/warning
                    continue;
                }

                ViewDefinition viewDefn = getWorkspaceManager().findViewDefinitionByNameIgnoreCase(dataserviceName, name);
                if (viewDefn != null) {
                    //sanity check
                    if (!name.equalsIgnoreCase(viewDefn.getName())) {
                        throw new AssertionError("imported view name conflicts with an existing view name");
                    }

                    //reuse the same id
                    viewDefn.clearState();
                    viewDefn.setUserDefined(false);
                    viewDefn.setDdl(null);
                    viewDefn.setDescription(null);
                } else {
                    viewDefn = new ViewDefinition(dataserviceName, name);
                }
                viewDefn.setComplete(true);
                FullyQualifiedName fqn = new FullyQualifiedName(Schema.getTypeName(), komodoSourceName);
                fqn.append(Schema.getChildType(t.getClass()), t.getName());
                viewDefn.addSourcePath(fqn.toString());

                String ddl = serviceVdbGenerator.getODataViewDdl(viewDefn);
                viewDefn.setDdl(ddl);
                viewDefn.setParsable(true);
                toSave.add(viewDefn);
            }

            for (ViewDefinition vd : getWorkspaceManager().saveAllViewDefinitions(toSave)) {
                result.addAttribute(vd.getName(), vd.getId());
            }

            dataservice.setModifiedAt(null);

            return result;
        });

        return kso;
    }

    /**
     * Get OData hostname from the buildStatus
     * @param buildStatus the BuildStatus
     * @return the odata hostname
     */
    private String getOdataHost(final BuildStatus buildStatus) {
        String odataHost = null;
        if(buildStatus != null) {
            List<RouteStatus> routeStatuses = buildStatus.routes();
            if(!routeStatuses.isEmpty()) {
                // Find Odata route if it exists
                for(RouteStatus routeStatus: routeStatuses) {
                    if(routeStatus.getProtocol() == ProtocolType.ODATA) {
                        odataHost = routeStatus.getHost();
                        break;
                    }
                }
            }
        }
        return odataHost;
    }


    /**
     * Update the specified Dataservice from the komodo repository
     * @param dataserviceName the dataservice name (cannot be empty)
     * @return a JSON representation of the new connection (never <code>null</code>)
     * @throws Exception
     */
    @RequestMapping(value = FS + V1Constants.DATA_SERVICE_PLACEHOLDER, method = RequestMethod.PUT, produces = {
            MediaType.APPLICATION_JSON_VALUE }, consumes = { MediaType.APPLICATION_JSON_VALUE })
    @ApiOperation(value = "Update data service")
    @ApiResponses(value = { @ApiResponse(code = 400, message = "An error has occurred.") })
    public KomodoStatusObject updateDataservice(
            @ApiParam(value = "Name of the data service", required = true)
            final @PathVariable("dataserviceName") String dataserviceName,
            @ApiParam(required = true) @RequestBody final RestDataVirtualization restDataservice) throws Exception {

        final String jsonDataserviceName = restDataservice.getName();
        // Error if the name is missing from the supplied json body
        if (StringUtils.isBlank(jsonDataserviceName)) {
            throw forbidden(RelationalMessages.Error.DATASERVICE_SERVICE_MISSING_NAME);
        }

        // Error if the name parameter is different than JSON name
        final boolean namesMatch = dataserviceName.equals(jsonDataserviceName);
        if (!namesMatch) {
            throw forbidden(DATASERVICE_SERVICE_SERVICE_NAME_ERROR, dataserviceName, jsonDataserviceName);
        }

        return kengine.runInTransaction(false, () -> {
            // Error if the repo already contains a dataservice with the supplied name.
            DataVirtualization existing = getWorkspaceManager().findDataVirtualization(restDataservice.getName());
            if (existing == null) {
                throw notFound( dataserviceName );
            }

            existing.setDescription(restDataservice.getDescription());
            KomodoStatusObject kso = new KomodoStatusObject("Update Dataservice Status"); //$NON-NLS-1$
            kso.addAttribute(dataserviceName, "Dataservice successfully updated"); //$NON-NLS-1$

            return kso;
        });
    }

    @RequestMapping(value = V1Constants.DATA_SERVICE_PLACEHOLDER + FS + "export", method = RequestMethod.GET, produces = {
            MediaType.MULTIPART_FORM_DATA_VALUE })
    @ApiOperation(value = "Find dataservice by name", response = RestDataVirtualization.class)
    @ApiResponses(value = { @ApiResponse(code = 404, message = "No Dataservice could be found with name"),
            @ApiResponse(code = 406, message = "Only JSON is returned by this operation"),
            @ApiResponse(code = 403, message = "An error has occurred.") })
    public ResponseEntity<StreamingResponseBody> exportDataservice(
            @ApiParam(value = "name of the dataservice",
            required = true) final @PathVariable("dataserviceName") String dataserviceName)
            throws Exception {

        DataVirtualizationV1Adapter result = kengine.runInTransaction(true, () -> {
            DataVirtualization dv = getWorkspaceManager().findDataVirtualization(dataserviceName);

            if (dv == null) {
                throw notFound(dataserviceName);
            }

            DataVirtualizationV1Adapter adapter = new DataVirtualizationV1Adapter(dv);

            List<? extends ViewDefinition> views = getWorkspaceManager().findViewDefinitions(dataserviceName);

            Map<String, SourceV1> sources = new LinkedHashMap<>();

            for (ViewDefinition view : views) {
                adapter.getViews().add(new ViewDefinitionV1Adapter(view));
                for (String path : view.getSourcePaths()) {
                    String connection = PathUtils.getOptions(path).get(0).getSecond();
                    if (sources.containsKey(connection)) {
                        continue;
                    }
                    TeiidDataSource tds = this.metadataService.findTeiidDatasource(connection);
                    if (tds != null) {
                        SourceV1 source = new SourceV1();
                        source.setSourceId(tds.getSyndesisId());
                        source.setName(tds.getName());
                        sources.put(connection, source);
                    }
                }
            }

            adapter.setSources(new ArrayList<>(sources.values()));

            return adapter;
        });

        StreamingResponseBody stream = out -> {
            ZipOutputStream zos = new ZipOutputStream(out);

            JsonFactory jsonFactory = new JsonFactory();
            jsonFactory.configure(JsonGenerator.Feature.AUTO_CLOSE_TARGET, false);
            ObjectMapper mapper = new ObjectMapper(jsonFactory);

            zos.putNextEntry(new ZipEntry("dv.json")); //$NON-NLS-1$
            mapper.writerWithDefaultPrettyPrinter().writeValue(zos, result);
            zos.closeEntry();

            zos.putNextEntry(new ZipEntry("dv-info.json")); //$NON-NLS-1$
            zos.write("{\"version\":1}".getBytes("UTF-8")); //$NON-NLS-1$ //$NON-NLS-2$
            zos.closeEntry();

            zos.close();
        };
        MultiValueMap<String, String> headers = new HttpHeaders();
        headers.add("Content-Disposition", "attachment; filename=\""+dataserviceName+"-export.zip\""); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
        return new ResponseEntity<StreamingResponseBody>(stream, headers, HttpStatus.OK);
    }

    @PostMapping()
    @ApiOperation(value = "Import a single data virtualization", response = String.class)
    public ResponseEntity<KomodoStatusObject> importDataservice(@ApiParam(value = "name of the dataservice")
            @RequestParam(name="virtualization", required=false) String virtualization,
            @RequestParam("file") MultipartFile file) throws Exception {

        final DataVirtualizationV1Adapter dv;
        try (InputStream is = file.getInputStream();) {
            ZipInputStream zis = new ZipInputStream(is);

            ZipEntry ze = zis.getNextEntry();
            while (ze != null && !ze.getName().equals("dv.json")) {
                ze = zis.getNextEntry();
            }
            if (ze == null) {
                throw new ResponseStatusException(HttpStatus.BAD_REQUEST, "zip does not contain dv.json");
            }

            ObjectMapper mapper = new ObjectMapper();
            mapper.configure(DeserializationFeature.FAIL_ON_UNKNOWN_PROPERTIES, false);
            dv = mapper.readValue(zis, DataVirtualizationV1Adapter.class);
        }

        DataVirtualization toImport = dv.getEntity();
        if (virtualization == null) {
            virtualization = toImport.getName();
        } else {
            toImport.setName(virtualization);
        }

        //TODO: validate the uuid or assign a new one
        //for now we just assign new to all objects

        for (ViewDefinitionV1Adapter adapter : dv.getViews()) {
            ViewDefinition entity = adapter.getEntity();
            entity.setId(null);
            entity.setDataVirtualizationName(virtualization);
        }

        KomodoStatusObject kso = new KomodoStatusObject("import result"); //$NON-NLS-1$

        try {
            kengine.runInTransaction(false, () -> {
                for (SourceV1 source : dv.getSources()) {
                    TeiidDataSource tds = metadataService.findTeiidDatasource(source.getName());
                    if (tds == null) {
                        //nothing with this name, check by id
                        SourceSchema schema = getWorkspaceManager().findSchemaBySourceId(source.getSourceId());
                        if (schema != null) {
                            kso.addAttribute(source.getName(), "a syndesis connection exists with the given id, but does not match the name"); //$NON-NLS-1$
                        } else {
                            kso.addAttribute(source.getName(), "no syndesis connection can be found"); //$NON-NLS-1$
                        }
                    } else {
                        if (tds.getSyndesisId().equals(source.getSourceId())) {
                            //presumably everything checks out
                            //however it seems like syndesis connection ids are simply sequential,
                            //so they may not be consistent across environments
                        } else {
                            kso.addAttribute(source.getName(), "a syndesis connection with the same name exists, but the ids do not match"); //$NON-NLS-1$
                        }
                    }
                }
                createDataservice(toImport.getName(), new RestDataVirtualization(toImport));

                for (ViewDefinitionV1Adapter adapter : dv.getViews()) {
                    ViewDefinition vd = adapter.getEntity();
                    utilService.upsertViewEditorState(vd);
                }
                return null;
            });
        } catch (DataIntegrityViolationException e) {
            throw error(HttpStatus.CONFLICT, RelationalMessages.Error.DATASERVICE_SERVICE_CREATE_ALREADY_EXISTS);
        }

        return new ResponseEntity<KomodoStatusObject>(kso, HttpStatus.OK);
    }

}
