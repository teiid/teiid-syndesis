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

import static org.komodo.spi.storage.StorageConnectorConstants.DOWNLOADABLE_PATH_PROPERTY;
import static org.komodo.spi.storage.StorageConnectorConstants.FILES_HOME_PATH_PROPERTY;
import static org.komodo.spi.storage.StorageConnectorConstants.FILE_PATH_PROPERTY;
import static org.komodo.spi.storage.StorageConnectorConstants.IMPORT_OVERWRITE_PROPERTY;

import java.io.File;
import java.io.FileInputStream;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.Properties;
import java.util.Set;

import javax.ws.rs.Consumes;
import javax.ws.rs.GET;
import javax.ws.rs.POST;
import javax.ws.rs.Path;
import javax.ws.rs.Produces;
import javax.ws.rs.core.Context;
import javax.ws.rs.core.HttpHeaders;
import javax.ws.rs.core.MediaType;
import javax.ws.rs.core.Response;
import javax.ws.rs.core.Response.Status;
import javax.ws.rs.core.UriInfo;

import org.komodo.importer.ImportMessages;
import org.komodo.importer.ImportOptions;
import org.komodo.importer.ImportOptions.OptionKeys;
import org.komodo.rest.KomodoRestException;
import org.komodo.rest.KomodoRestV1Application.V1Constants;
import org.komodo.rest.KomodoService;
import org.komodo.rest.relational.RelationalMessages;
import org.komodo.rest.relational.json.KomodoJsonMarshaller;
import org.komodo.rest.relational.response.ImportExportStatus;
import org.komodo.rest.relational.response.KomodoStorageAttributes;
import org.komodo.rest.relational.response.RestStorageType;
import org.komodo.spi.repository.DocumentType;
import org.komodo.spi.repository.Exportable;
import org.komodo.spi.repository.KomodoObject;
import org.komodo.spi.repository.Repository;
import org.komodo.spi.repository.Repository.UnitOfWork;
import org.komodo.spi.repository.Repository.UnitOfWork.State;
import org.komodo.spi.storage.StorageConnector.Descriptor;
import org.komodo.spi.storage.StorageReference;
import org.komodo.spi.storage.StorageService;
import org.komodo.storage.StorageServiceProvider;
import org.komodo.utils.FileUtils;
import org.komodo.utils.KLog;
import org.komodo.utils.StringUtils;
import org.springframework.stereotype.Component;

import io.swagger.annotations.Api;
import io.swagger.annotations.ApiOperation;
import io.swagger.annotations.ApiParam;
import io.swagger.annotations.ApiResponse;
import io.swagger.annotations.ApiResponses;

/**
 * A Komodo REST service for import / export artifacts into / out of the workspace
 */
@Component
@Path( V1Constants.IMPORT_EXPORT_SEGMENT )
@Api( tags = {V1Constants.IMPORT_EXPORT_SEGMENT} )
public class KomodoImportExportService extends KomodoService {

    /**
     * Property key used to specify the name of a git repository
     * contained in the user's profile.
     */
    public static final String PROFILE_REPOSITORY_NAME = "profile-repository-name";

    private Response checkStorageAttributes(KomodoStorageAttributes sta,
                                            List<MediaType> mediaTypes) throws Exception {
        Map<String, String> parameters = sta.getParameters();
        if (sta == null ||
            (sta.getStorageType() == null && sta.getArtifactPath() == null && parameters == null)) {
            return createErrorResponseWithForbidden(mediaTypes, RelationalMessages.Error.IMPORT_EXPORT_SERVICE_NO_PARAMETERS_ERROR);
        }

        Set<String> supportedTypes = StorageServiceProvider.getInstance().getSupportedStorageTypes();
        if (! supportedTypes.contains(sta.getStorageType())) {
            return createErrorResponseWithForbidden(mediaTypes, RelationalMessages.Error.IMPORT_EXPORT_SERVICE_UNSUPPORTED_TYPE_ERROR);
        }

        StorageService storageService = StorageServiceProvider.getInstance().getStorageService(sta.getStorageType());

        //
        // Check the required descriptors exist
        //
        for (Descriptor descriptor: storageService.getDescriptors()) {
            if (! descriptor.isRequired())
                continue;

            if (FILE_PATH_PROPERTY.equals(descriptor.getName()))
                continue; // This is handled separately in import/export and can be populated if missing

            if (! parameters.containsKey(descriptor.getName())) {
                return createErrorResponseWithForbidden(mediaTypes, RelationalMessages.Error.IMPORT_EXPORT_SERVICE_MISSING_PARAMETER_ERROR,
                                                        descriptor.getName());
            }
        }

        //
        // Decode any encoded parameters
        //
        for (Descriptor descriptor : storageService.getDescriptors()) {
            if (! descriptor.isEncoded())
                continue;

            if (! parameters.containsKey(descriptor.getName()))
                continue;

            String value = parameters.get(descriptor.getName());

            value = new String(decode(value));
            sta.setParameter(descriptor.getName(), value);
        }

        return Response.ok().build();
    }

    /**
     * Exports an artifact from the workspace.
     *
     * @param headers
     *        the request headers (never <code>null</code>)
     * @param uriInfo
     *        the request URI information (never <code>null</code>)
     * @param export attributes
     *        the export attributes JSON representation (cannot be <code>null</code>)
     * @return a JSON document including Base64 content of the file
     *                  (never <code>null</code>)
     * @throws KomodoRestException
     *         if there is a problem with the export
     */
    @POST
    @Path(V1Constants.EXPORT)
    @Produces( MediaType.APPLICATION_JSON )
    @Consumes ( { MediaType.APPLICATION_JSON } )
    @ApiOperation(value = "Exports an artifact using parameters provided in the request body",
                             response = ImportExportStatus.class)
    @ApiResponses(value = {
        @ApiResponse(code = 406, message = "Only JSON is returned by this operation"),
        @ApiResponse(code = 403, message = "An error has occurred.")
    })
    public Response exportArtifact( final @Context HttpHeaders headers,
                             final @Context UriInfo uriInfo,
                             @ApiParam(
                                       value = "" +
                                               "JSON of the possible storage attributes:<br>" +
                                               OPEN_PRE_TAG +
                                               OPEN_BRACE + BR +
                                               NBSP + "storageType: \"Either 'file' or 'git'\"" + COMMA + BR +
                                               NBSP + "dataPath: \"Path of the object to be exported\"" + COMMA + BR +
                                               NBSP + "documentType: \"Expected destination export file type\"" + BR +
                                               NBSP + OPEN_PRE_CMT + "(Used to help determine the expected file type, " +
                                               "eg. zip, xml, directory)" + CLOSE_PRE_CMT + BR +
                                               NBSP + "parameters: " + OPEN_BRACE + BR +
                                               NBSP + NBSP + FILES_HOME_PATH_PROPERTY + ": \"Path to the parent " +
                                               "directory of the export files\"" + COMMA + BR +
                                               NBSP + NBSP + FILE_PATH_PROPERTY + ": \"Relative path, inc. name, " +
                                               "of the destination exported file\"" + COMMA + BR +
                                               NBSP + NBSP + OPEN_PRE_CMT +  "(Optional if documentType specified)" + CLOSE_PRE_CMT + BR +
                                               NBSP + NBSP + DOWNLOADABLE_PATH_PROPERTY + ": \"Should a file be " +
                                               "downloadable once exported\"" + COMMA + BR +
                                               NBSP + NBSP + "useTabs: \"Should tabs be used in exporting xml\"" + COMMA + BR +
                                               NBSP + NBSP + "excludeTableConstraints: \"Should table constraints " +
                                               "be excluded when exporting DDL\"" + COMMA + BR +
                                               NBSP + NBSP + OPEN_PRE_CMT + "(Further parameters are specific to storage type. " +
                                               "see /importexport/availableStorageType REST link)" + CLOSE_PRE_CMT + BR +
                                               NBSP + CLOSE_BRACE + BR +
                                               CLOSE_BRACE +
                                               CLOSE_PRE_TAG,
                                       required = true
                             )
                             final String storageAttributes) throws KomodoRestException {
        SecurityPrincipal principal = checkSecurityContext(headers);
        if (principal.hasErrorResponse())
            return principal.getErrorResponse();

        List<MediaType> mediaTypes = headers.getAcceptableMediaTypes();
        if (! isAcceptable(mediaTypes, MediaType.APPLICATION_JSON_TYPE))
            return notAcceptableMediaTypesBuilder().build();

        KomodoStorageAttributes sta;
        try {
            sta = KomodoJsonMarshaller.unmarshall(storageAttributes, KomodoStorageAttributes.class);

            Response response = checkStorageAttributes(sta, mediaTypes);
            if (response.getStatus() != Status.OK.getStatusCode())
                return response;

        } catch (Exception ex) {
            return createErrorResponseWithForbidden(mediaTypes, ex, RelationalMessages.Error.IMPORT_EXPORT_SERVICE_REQUEST_PARSING_ERROR);
        }

        ImportExportStatus status = new ImportExportStatus();
        UnitOfWork uow = null;
        try {
            uow = createTransaction(principal, "exportFromWorkspace", true); //$NON-NLS-1$
            Repository repo = this.kengine.getDefaultRepository();
            String artifactPath = sta.getArtifactPath();
            KomodoObject kObject = repo.getFromWorkspace(uow, artifactPath);
            if (kObject == null) {
                return createErrorResponseWithForbidden(mediaTypes, RelationalMessages.Error.IMPORT_EXPORT_SERVICE_NO_ARTIFACT_ERROR, artifactPath);
            }

            Exportable artifact = getWorkspaceManager(uow).resolve(uow, kObject, Exportable.class);
            if (artifact == null) {
                return createErrorResponseWithForbidden(mediaTypes, RelationalMessages.Error.IMPORT_EXPORT_SERVICE_ARTIFACT_NOT_EXPORTABLE_ERROR, artifactPath);
            }

            DocumentType documentType = artifact.getDocumentType(uow);
            Properties parameters = sta.convertParameters();
            if (! parameters.containsKey(FILE_PATH_PROPERTY)) {
                String fileName = documentType.fileName(artifact.getName(uow));
                parameters.setProperty(FILE_PATH_PROPERTY, fileName);
            }

            status.setName(artifact.getName(uow));
            status.setType(documentType.toString());

            String downloadable = getWorkspaceManager(uow).exportArtifact(uow, artifact, sta.getStorageType(), parameters);

            //
            // 2 Return possibilities:
            // a) Artifact exported to storage but not returned in response
            // b) Artifact available at server file location so return as content
            //
            status.setDownloadable(downloadable != null);

            applyContent(status, downloadable);

            status.setSuccess(true);

            return commit( uow, mediaTypes, status );

        } catch (final Exception e) {
            if ((uow != null) && (uow.getState() != State.ROLLED_BACK)) {
                uow.rollback();
            }

            return createErrorResponse(Status.FORBIDDEN, mediaTypes, e,
                                       RelationalMessages.Error.IMPORT_EXPORT_SERVICE_EXPORT_ERROR,
                                       sta.getArtifactPath(), sta.getStorageType());
        }
    }

    private void applyContent(ImportExportStatus status, String downloadable) throws Exception {
        if (downloadable == null)
            return;

        FileInputStream stream = null;
        try {
            File downloadableFile = new File(downloadable);
            stream = new FileInputStream(downloadableFile);

            status.setDownloadableSize(downloadableFile.length());

            byte content[] = new byte[(int)downloadableFile.length()];
            stream.read(content);
            String encContent = encode(content);
            status.setContent(encContent);

            KLog.getLogger().debug("Encrypted content of " + downloadableFile.getAbsolutePath() + ": " + encContent + " SIZE: "
                                  + downloadableFile.length());

        } finally {
            stream.close();
        }
    }

    /**
     * Imports an artifact into the workspace.
     *
     * @param headers
     *        the request headers (never <code>null</code>)
     * @param uriInfo
     *        the request URI information (never <code>null</code>)
     * @param import attributes
     *        the import attributes JSON representation (cannot be <code>null</code>)
     * @return a status object indicating success / failure of the import
     * @throws KomodoRestException
     *         if there is a problem with the import
     */
    @POST
    @Path(V1Constants.IMPORT)
    @Produces( MediaType.APPLICATION_JSON )
    @Consumes ( { MediaType.APPLICATION_JSON } )
    @ApiOperation(value = "Imports an artifact using parameters provided in the request body",
                             response = ImportExportStatus.class)
    @ApiResponses(value = {
        @ApiResponse(code = 406, message = "Only JSON is returned by this operation"),
        @ApiResponse(code = 403, message = "An error has occurred.")
    })
    public Response importArtifact( final @Context HttpHeaders headers,
                             final @Context UriInfo uriInfo,
                             @ApiParam(
                                       value = "" +
                                               "JSON of the possible storage attributes:<br>" +
                                               OPEN_PRE_TAG +
                                               OPEN_BRACE + BR +
                                               NBSP + "storageType: \"Either 'file' or 'git'\"" + COMMA + BR +
                                               NBSP + "content: \"Base64-encoded byte data of the file to import\"" + COMMA + BR +
                                               NBSP + OPEN_PRE_CMT + "(If defined then this overrides 'files-home' & 'file-path' properties)" + CLOSE_PRE_CMT + BR +
                                               NBSP + "dataPath: \"Destination object path\"" + COMMA + BR +
                                               NBSP + OPEN_PRE_CMT + "(If not specified then located under the workspace)" + CLOSE_PRE_CMT + BR +
                                               NBSP + "documentType: \"Type of the file being imported\"" + BR +
                                               NBSP + OPEN_PRE_CMT + "(Required for content type, eg. zip, xml, directory)" + CLOSE_PRE_CMT + BR +
                                               NBSP + "parameters: " + OPEN_BRACE + BR +
                                               NBSP + NBSP + FILES_HOME_PATH_PROPERTY + ": \"Path to the parent " +
                                               "location of the file to import\"" + COMMA + BR +
                                               NBSP + NBSP + FILE_PATH_PROPERTY + ": \"Relative path, inc. name, " +
                                               "of the file to import\"" + BR +
                                               NBSP + NBSP + OPEN_PRE_CMT +  "(Further parameters are specific to storage type. " +
                                               "see /importexport/availableStorageType REST link)" + CLOSE_PRE_CMT + BR +
                                               NBSP + CLOSE_BRACE + BR +
                                               CLOSE_BRACE +
                                               CLOSE_PRE_TAG,
                                       required = true
                             )
                             final String storageAttributes) throws KomodoRestException {
        SecurityPrincipal principal = checkSecurityContext(headers);
        if (principal.hasErrorResponse())
            return principal.getErrorResponse();

        List<MediaType> mediaTypes = headers.getAcceptableMediaTypes();
        if (! isAcceptable(mediaTypes, MediaType.APPLICATION_JSON_TYPE))
            return notAcceptableMediaTypesBuilder().build();

        KomodoStorageAttributes sta;
        try {
            sta = KomodoJsonMarshaller.unmarshall(storageAttributes, KomodoStorageAttributes.class);
            Response response = checkStorageAttributes(sta, mediaTypes);
            if (response.getStatus() != Status.OK.getStatusCode())
                return response;

        } catch (Exception ex) {
            return createErrorResponseWithForbidden(mediaTypes, ex, RelationalMessages.Error.IMPORT_EXPORT_SERVICE_REQUEST_PARSING_ERROR);
        }

        File cttFile = null;
        ImportExportStatus status = new ImportExportStatus();
        UnitOfWork uow = null;
        try {
            if (sta.getContent() != null) {
                //
                // Content has been provided so need to outline its location
                // for the storage connector to utilise
                //
                byte[] content = decode(sta.getContent());
                String tempDir = FileUtils.tempDirectory();
                String fileName = content.hashCode() + DOT + sta.getDocumentType();
                cttFile = new File(tempDir, fileName);

                FileUtils.write(content, cttFile);

                // Ensure the new location of the file is conveyed to the storage plugin
                sta.setParameter(FILES_HOME_PATH_PROPERTY, tempDir);
                sta.setParameter(FILE_PATH_PROPERTY, fileName);
            }

            Properties parameters = sta.convertParameters();
            if (! parameters.containsKey(FILE_PATH_PROPERTY)) {
                return createErrorResponse(Status.FORBIDDEN, mediaTypes, RelationalMessages.Error.IMPORT_EXPORT_SERVICE_NO_FILE_PATH_ERROR);
            }
            Repository repo = this.kengine.getDefaultRepository();
            uow = createTransaction(principal, "importToWorkspace", false); //$NON-NLS-1$
            KomodoObject importTarget = repo.komodoWorkspace(uow);

            // If artifact path is supplied, it is the target.  Otherwise default to workspace
            String artifactPath = sta.getArtifactPath();
            if(!StringUtils.isEmpty(artifactPath)) {
            	importTarget = repo.getFromWorkspace(uow, artifactPath);
                if (importTarget == null) {
                    return createErrorResponseWithForbidden(mediaTypes, RelationalMessages.Error.IMPORT_EXPORT_SERVICE_NO_ARTIFACT_ERROR, artifactPath);
                }
            }

            StorageReference storageRef = new StorageReference(sta.getStorageType(),
                                                                                   parameters,
                                                                                   new DocumentType(sta.getDocumentType()));

            // Set desired overwrite setting in options
            ImportOptions importOptions = new ImportOptions();
            if(parameters.containsKey(IMPORT_OVERWRITE_PROPERTY)) {
                String importOverwrite = parameters.getProperty(IMPORT_OVERWRITE_PROPERTY);
                // RETURN supplied
                if( importOverwrite.equals(ImportOptions.ExistingNodeOptions.RETURN.name()) ) {
                    importOptions.setOption(OptionKeys.HANDLE_EXISTING, ImportOptions.ExistingNodeOptions.RETURN);
                // CREATE_NEW supplied
                } else if( importOverwrite.equals(ImportOptions.ExistingNodeOptions.CREATE_NEW.name()) ) {
                    importOptions.setOption(OptionKeys.HANDLE_EXISTING, ImportOptions.ExistingNodeOptions.CREATE_NEW);
                // OVERWRITE supplied or no match
                } else {
                    importOptions.setOption(OptionKeys.HANDLE_EXISTING, ImportOptions.ExistingNodeOptions.OVERWRITE);
                }
            } else {
                importOptions.setOption(OptionKeys.HANDLE_EXISTING, ImportOptions.ExistingNodeOptions.OVERWRITE);
            }

            ImportMessages messages = getWorkspaceManager(uow).importArtifact(uow, importTarget, storageRef, importOptions);
            if (messages.hasError()) {
                return createErrorResponseWithForbidden(mediaTypes, RelationalMessages.Error.IMPORT_EXPORT_SERVICE_IMPORT_ARTIFACT_ERROR, messages.errorMessagesToString());
            }

            status.setSuccess(true);
            status.setName(storageRef.getRelativeRef());
            status.setMessage(RelationalMessages.getString( RelationalMessages.Info.IMPORT_EXPORT_SERVICE_IMPORT_SUCCESS_MESSAGE, importOptions.getOption(OptionKeys.NAME) ));

            if(sta.getDocumentType().equals(DocumentType.JAR.toString())) {
                String driverName = storageRef.getParameters().getProperty(StorageReference.DRIVER_NAME_KEY);
                if(StringUtils.isBlank(driverName)) {
                    driverName = StorageReference.DRIVER_NAME_DEFAULT;
                }
                status.setName(driverName);
            }

            status.setType(sta.getDocumentType().toString());

            return commit( uow, mediaTypes, status );

        } catch (final Exception e) {
            if ((uow != null) && (uow.getState() != State.ROLLED_BACK)) {
                uow.rollback();
            }

            return createErrorResponse(Status.FORBIDDEN, mediaTypes, e,
                                       RelationalMessages.Error.IMPORT_EXPORT_SERVICE_IMPORT_ERROR,
                                       sta.getStorageType());
        } finally {
            //
            // Clean up the temporary file if applicable
            //
            if (cttFile != null)
                cttFile.delete();
        }
    }

    /**
     * Gets the types of storage available for import/export
     *
     * @param headers
     *        the request headers (never <code>null</code>)
     * @param uriInfo
     *        the request URI information (never <code>null</code>)
     * @return the collection of the storage types
     * @throws KomodoRestException
     *         if there is a problem with the operation
     */
    @GET
    @Path(V1Constants.STORAGE_TYPES)
    @Produces( MediaType.APPLICATION_JSON )
    @ApiOperation(value = "Returns the collection of available storage types used for import/export",
                             response = RestStorageType[].class)
    @ApiResponses(value = {
        @ApiResponse(code = 406, message = "Only JSON is returned by this operation"),
        @ApiResponse(code = 403, message = "An error has occurred.")
    })
    public Response storageTypes( final @Context HttpHeaders headers,
                             final @Context UriInfo uriInfo) throws KomodoRestException {

        SecurityPrincipal principal = checkSecurityContext(headers);
        if (principal.hasErrorResponse())
            return principal.getErrorResponse();

        List<MediaType> mediaTypes = headers.getAcceptableMediaTypes();
        if (! isAcceptable(mediaTypes, MediaType.APPLICATION_JSON_TYPE))
            return notAcceptableMediaTypesBuilder().build();

        try {
            StorageServiceProvider storageServiceProvider = StorageServiceProvider.getInstance();
            Set<String> storageTypes = storageServiceProvider.getSupportedStorageTypes();
            if (storageTypes == null || storageTypes.isEmpty()) {
                return Response.noContent().build();
            }

            List<RestStorageType> restStorageTypes = new ArrayList<>(storageTypes.size());
            for (String storageType : storageTypes) {
                StorageService storageService = storageServiceProvider.getStorageService(storageType);
                String description = storageService.getDescription();
                Set<Descriptor> descriptors = storageService.getDescriptors();

                RestStorageType type = new RestStorageType(storageType, description, descriptors);
                restStorageTypes.add(type);
            }

            UnitOfWork uow = createTransaction(principal, "getStorageTypes", true); //$NON-NLS-1$
            return commit(uow, mediaTypes, restStorageTypes);

        } catch (Exception e) {
            return createErrorResponseWithForbidden(mediaTypes, e,
                                       RelationalMessages.Error.IMPORT_EXPORT_SERVICE_STORAGE_TYPES_ERROR);
        }
    }

}
