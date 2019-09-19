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

import java.util.ArrayList;
import java.util.List;

import org.komodo.KException;
import org.komodo.StringConstants;
import org.komodo.datavirtualization.DataVirtualization;
import org.komodo.datavirtualization.ViewDefinition;
import org.komodo.metadata.TeiidVdb;
import org.komodo.openshift.BuildStatus;
import org.komodo.openshift.PublishConfiguration;
import org.komodo.openshift.TeiidOpenShiftClient;
import org.komodo.rest.AuthHandlingFilter.OAuthCredentials;
import org.komodo.rest.KomodoService;
import org.komodo.rest.V1Constants;
import org.komodo.rest.datavirtualization.KomodoStatusObject;
import org.komodo.rest.datavirtualization.PublishRequestPayload;
import org.komodo.rest.datavirtualization.RelationalMessages;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.HttpStatus;
import org.springframework.http.MediaType;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.web.bind.annotation.RestController;
import org.springframework.web.server.ResponseStatusException;
import org.teiid.adminapi.impl.VDBMetaData;

import io.swagger.annotations.Api;
import io.swagger.annotations.ApiOperation;
import io.swagger.annotations.ApiParam;
import io.swagger.annotations.ApiResponse;
import io.swagger.annotations.ApiResponses;

@RestController
@RequestMapping( V1Constants.APP_PATH+V1Constants.FS+V1Constants.METADATA_SEGMENT )
@Api( tags = {V1Constants.METADATA_SEGMENT} )
public class KomodoPublishingService extends KomodoService {

    @Autowired
    private TeiidOpenShiftClient openshiftClient;

    @Autowired
    private KomodoMetadataService komodoMetadataService;

    @RequestMapping(value = V1Constants.PUBLISH, method = RequestMethod.GET, produces = { MediaType.APPLICATION_JSON_VALUE })
    @ApiOperation(value = "Gets the published virtualization services",
        response = BuildStatus.class,
        responseContainer = "List")
    @ApiResponses(value = { @ApiResponse(code = 403, message = "An error has occurred.") })
    public List<BuildStatus> getVirtualizations() throws KException {

        List<BuildStatus> result = new ArrayList<>();
        for (String name : getWorkspaceManager().findDataVirtualizationNames()) {
            result.add(this.openshiftClient.getVirtualizationStatus(name));
        }

        return result;
    }

    @RequestMapping(value = V1Constants.PUBLISH + StringConstants.FS
            + V1Constants.VIRTUALIZATION_PLACEHOLDER, method = RequestMethod.GET, produces = { MediaType.APPLICATION_JSON_VALUE })
    @ApiOperation(value = "Find Build Status of Virtualization", response = BuildStatus.class)
    @ApiResponses(value = {
        @ApiResponse(code = 404, message = "No virtualization could be found with name"),
        @ApiResponse(code = 406, message = "Only JSON returned by this operation"),
        @ApiResponse(code = 403, message = "An error has occurred.")
    })
    public BuildStatus getVirtualizationStatus(
            @ApiParam(value = "Name of the virtualization", required = true)
            final @PathVariable(value = "virtualization", required=true) String virtualization) {
        BuildStatus status = this.openshiftClient.getVirtualizationStatus(virtualization);

        return status;
    }

    @RequestMapping(value = V1Constants.PUBLISH_LOGS + StringConstants.FS
            + V1Constants.VIRTUALIZATION_PLACEHOLDER, method = RequestMethod.GET, produces = { MediaType.APPLICATION_JSON_VALUE })
    @ApiOperation(value = "Find Publish Logs of Virtualization", response = KomodoStatusObject.class)
    @ApiResponses(value = {
        @ApiResponse(code = 404, message = "No virtualization could be found with name"),
        @ApiResponse(code = 406, message = "Only JSON returned by this operation"),
        @ApiResponse(code = 403, message = "An error has occurred.")
    })
    public KomodoStatusObject getVirtualizationLogs(
            @ApiParam(value = "Name of the virtualization")
            final @PathVariable(value = "virtualization", required = true) String virtualization) {

        KomodoStatusObject status = new KomodoStatusObject("Logs for " + virtualization); //$NON-NLS-1$

        String log = this.openshiftClient.getVirtualizationLog(virtualization);
        status.addAttribute("log", log); //$NON-NLS-1$
        return status;
    }

    @RequestMapping(value = V1Constants.PUBLISH + StringConstants.FS
            + V1Constants.VIRTUALIZATION_PLACEHOLDER, method = RequestMethod.DELETE, produces = { MediaType.APPLICATION_JSON_VALUE })
    @ApiOperation(value = "Delete Virtualization Service by virtualization name",response = BuildStatus.class)
    @ApiResponses(value = {
        @ApiResponse(code = 404, message = "No virtualization could be found with name"),
        @ApiResponse(code = 406, message = "Only JSON returned by this operation"),
        @ApiResponse(code = 403, message = "An error has occurred.")
    })
    public BuildStatus deleteVirtualization(
            @ApiParam(value = "Name of the virtualization")
            final @PathVariable(value = "virtualization", required = true) String virtualization) {
        BuildStatus status = this.openshiftClient.deleteVirtualization(virtualization);
        return status;
    }

    @RequestMapping(value = V1Constants.PUBLISH, method = RequestMethod.POST, produces = { MediaType.APPLICATION_JSON_VALUE })
    @ApiOperation(value = "Publish Virtualization Service", response = KomodoStatusObject.class)
    @ApiResponses(value = {
        @ApiResponse(code = 404, message = "No Dataservice could be found with name"),
        @ApiResponse(code = 406, message = "Only JSON returned by this operation"),
        @ApiResponse(code = 403, message = "An error has occurred.")
    })
    public KomodoStatusObject publishVirtualization(
            @ApiParam(value = "JSON properties:<br>" + OPEN_PRE_TAG + OPEN_BRACE + BR + NBSP
                    + "\"name\":      \"Name of the Dataservice\"" + BR
                    + "\"cpu-units\": \"(optional) Number of CPU units to allocate. 100 is 0.1 CPU (default 500)\"" + BR
                    + "\"memory\":    \"(optional) Amount memory to allocate in MB (default 1024)\"" + BR
                    + "\"disk-size\": \"(optional) Amount disk allocated in GB (default 20)\"" + BR
                    + "\"enable-odata\": \"(optional) Enable OData interface. true|false (default true)\"" + BR
                    + CLOSE_BRACE
                    + CLOSE_PRE_TAG) @RequestBody(required = true) final PublishRequestPayload payload) throws Exception {
        //
        // Error if there is no name attribute defined
        //
        if (payload.getName() == null) {
            throw forbidden(RelationalMessages.Error.VDB_NAME_NOT_PROVIDED);
        }

        return kengine.runInTransaction(true, ()-> {
            DataVirtualization dataservice = getWorkspaceManager().findDataVirtualization(payload.getName());
            if (dataservice == null) {
                throw notFound(payload.getName());
            }

            TeiidVdb vdb = komodoMetadataService.updatePreviewVdb(dataservice.getName());

            if (vdb == null || !vdb.hasLoaded()) {
                throw new ResponseStatusException(HttpStatus.SERVICE_UNAVAILABLE);
            }

            KomodoStatusObject status = new KomodoStatusObject();

            List<String> errors = vdb.getValidityErrors();
            if (!errors.isEmpty()) {
                status.addAttribute("error", errors.iterator().next());  //$NON-NLS-1$
                return status;
            }

            status.addAttribute("Publishing", "Operation initiated");  //$NON-NLS-1$//$NON-NLS-2$

            final OAuthCredentials creds = getAuthenticationToken();

            List<? extends ViewDefinition> editorStates = getWorkspaceManager().findViewDefinitions(dataservice.getName());

            //check for unparsable - alternatively we could put this on the preview vdb
            for (ViewDefinition vd : editorStates) {
                if (vd.isComplete() && !vd.isParsable()) {
                    status.addAttribute("error", vd.getName() + " is not parsable");  //$NON-NLS-1$ //$NON-NLS-2$
                    return status;
                }
            }

            //use the preview vdb to build the needed metadata
            VDBMetaData theVdb = new ServiceVdbGenerator(komodoMetadataService).createServiceVdb(dataservice.getName(), vdb, editorStates);

            // the properties in this class can be exposed for user input
            PublishConfiguration config = new PublishConfiguration();
            config.setVDB(theVdb);
            config.setOAuthCredentials(creds);
            config.setEnableOData(payload.getEnableOdata());
            config.setContainerDiskSize(payload.getDiskSize());
            config.setContainerMemorySize(payload.getMemory());
            config.setCpuUnits(payload.getCpuUnits());
            BuildStatus buildStatus = openshiftClient.publishVirtualization(config);

            status.addAttribute("OpenShift Name", buildStatus.getOpenShiftName()); //$NON-NLS-1$
            status.addAttribute("Build Status", buildStatus.status().name()); //$NON-NLS-1$
            status.addAttribute("Build Status Message", buildStatus.statusMessage()); //$NON-NLS-1$

            //
            // Return the status from this request. Otherwise, monitor using #getVirtualizations()
            //
            return status;
        });
    }

}
