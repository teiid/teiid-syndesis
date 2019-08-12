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

import static org.komodo.rest.datavirtualization.RelationalMessages.Error.VIEW_NAME_EXISTS;

import javax.ws.rs.GET;
import javax.ws.rs.Path;
import javax.ws.rs.PathParam;
import javax.ws.rs.Produces;
import javax.ws.rs.core.Context;
import javax.ws.rs.core.HttpHeaders;
import javax.ws.rs.core.MediaType;
import javax.ws.rs.core.Response;
import javax.ws.rs.core.UriInfo;

import org.komodo.StringConstants;
import org.komodo.datavirtualization.ViewDefinition;
import org.komodo.rest.KomodoService;
import org.komodo.rest.V1Constants;
import org.komodo.rest.datavirtualization.RelationalMessages;
import org.komodo.utils.StringNameValidator;
import org.springframework.stereotype.Component;

import io.swagger.annotations.Api;
import io.swagger.annotations.ApiOperation;
import io.swagger.annotations.ApiParam;
import io.swagger.annotations.ApiResponse;
import io.swagger.annotations.ApiResponses;

/**
 * A Komodo REST service for obtaining VDB information from the workspace.
 */
@Component
@Path(V1Constants.WORKSPACE_SEGMENT + StringConstants.FORWARD_SLASH +
           V1Constants.VDBS_SEGMENT)
@Api(tags = {V1Constants.VDBS_SEGMENT})
public final class KomodoVdbService extends KomodoService {

    private static final StringNameValidator VALIDATOR = new StringNameValidator();

    /**
     * @param headers
     *        the request headers (never <code>null</code>)
     * @param uriInfo
     *        the request URI information (never <code>null</code>)
     * @param vdbName
     *        the id of the Vdb being retrieved (cannot be empty)
     * @param modelName
     *        the id of the Model being retrieved (cannot be empty)
	 * @param viewName
	 *        the view name being validated (cannot be empty)
	 * @return the response (never <code>null</code>) with an entity that is
	 *         either an empty string, when the name is valid, or an error
	 *         message
     * @throws Exception 
     */
    @GET
    @Path( V1Constants.VDB_PLACEHOLDER + StringConstants.FORWARD_SLASH +
                V1Constants.MODELS_SEGMENT + StringConstants.FORWARD_SLASH +
                V1Constants.MODEL_PLACEHOLDER + StringConstants.FORWARD_SLASH +
                V1Constants.VIEWS_SEGMENT + StringConstants.FORWARD_SLASH +
                V1Constants.NAME_VALIDATION_SEGMENT + StringConstants.FORWARD_SLASH + V1Constants.VIEW_PLACEHOLDER)
    @Produces( { MediaType.TEXT_PLAIN } )
    @ApiOperation( value = "Returns an error message if the view name is invalid" )
    @ApiResponses( value = {
            @ApiResponse( code = 400, message = "The URI cannot contain encoded slashes or backslashes." ),
            @ApiResponse( code = 403, message = "An unexpected error has occurred." ),
            @ApiResponse( code = 404, message = "No vdb could be found with name" ),
            @ApiResponse( code = 500, message = "The view name cannot be empty." )
    } )
    public Response validateViewName( final @Context HttpHeaders headers,
                                      final @Context UriInfo uriInfo,
                                      @ApiParam(value = "Name of the Vdb", required = true)
                                      final @PathParam( "virtualization" ) String virtualization,
                                      @ApiParam(value = "Name of the Model to get its tables", required = true)
                                      final @PathParam( "viewName" ) String viewName ) throws Exception {

        String principal = checkSecurityContext(headers);

        final String errorMsg = VALIDATOR.checkValidName( viewName );

        // a name validation error occurred
        if ( errorMsg != null ) {
            return Response.ok().entity( errorMsg ).build();
        }

        return runInTransaction(principal, "validateViewName", true, ()-> {
            ViewDefinition vd = getWorkspaceManager().findViewDefinitionByNameIgnoreCase(virtualization, viewName);
            
        	if (vd != null) {
                // name is the same as an existing View
        		return Response.ok()
                        .entity( RelationalMessages.getString( VIEW_NAME_EXISTS ) )
                        .build();
        	}
            
        	// name is valid
        	return Response.ok().build();
        }) ; //$NON-NLS-1$
    }

}
