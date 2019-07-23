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

import static org.komodo.rest.relational.RelationalMessages.Error.VIEW_NAME_EXISTS;
import static org.komodo.rest.relational.RelationalMessages.Error.VIEW_NAME_VALIDATION_ERROR;

import java.util.List;

import javax.ws.rs.DELETE;
import javax.ws.rs.GET;
import javax.ws.rs.Path;
import javax.ws.rs.PathParam;
import javax.ws.rs.Produces;
import javax.ws.rs.core.Context;
import javax.ws.rs.core.HttpHeaders;
import javax.ws.rs.core.MediaType;
import javax.ws.rs.core.Response;
import javax.ws.rs.core.UriInfo;

import org.komodo.relational.WorkspaceManager;
import org.komodo.relational.vdb.Vdb;
import org.komodo.rest.KomodoRestException;
import org.komodo.rest.KomodoRestV1Application.V1Constants;
import org.komodo.rest.KomodoService;
import org.komodo.rest.relational.RelationalMessages;
import org.komodo.rest.relational.dataservice.RestDataservice;
import org.komodo.rest.relational.response.KomodoStatusObject;
import org.komodo.spi.StringConstants;
import org.komodo.spi.repository.UnitOfWork;
import org.komodo.spi.repository.UnitOfWork.State;
import org.komodo.utils.StringNameValidator;
import org.komodo.utils.StringUtils;
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
     * Delete the specified Vdb from the komodo repository
     * @param headers
     *        the request headers (never <code>null</code>)
     * @param uriInfo
     *        the request URI information (never <code>null</code>)
     * @param vdbName
     *        the name of the Vdb to be removed (cannot be <code>null</code>)
     * @return a JSON document representing the results of the removal
     * @throws KomodoRestException
     *         if there is a problem performing the delete
     */
    @DELETE
    @Path("{vdbName}")
    @Produces( MediaType.APPLICATION_JSON )
    @ApiOperation(value = "Delete a vdb from the workspace")
    @ApiResponses(value = {
        @ApiResponse(code = 406, message = "Only JSON is returned by this operation"),
        @ApiResponse(code = 403, message = "An error has occurred.")
    })
    public Response deleteVdb( final @Context HttpHeaders headers,
                               final @Context UriInfo uriInfo,
                               @ApiParam(
                                         value = "Name of the Vdb to be removed",
                                         required = true
                               )
                               final @PathParam( "vdbName" ) String vdbName) throws KomodoRestException {

        SecurityPrincipal principal = checkSecurityContext(headers);
        if (principal.hasErrorResponse())
            return principal.getErrorResponse();

        List<MediaType> mediaTypes = headers.getAcceptableMediaTypes();
        if (! isAcceptable(mediaTypes, MediaType.APPLICATION_JSON_TYPE))
            return notAcceptableMediaTypesBuilder().build();

        // Error if the vdb name is missing
        if (StringUtils.isBlank( vdbName )) {
            return createErrorResponseWithForbidden(mediaTypes, RelationalMessages.Error.VDB_SERVICE_DELETE_MISSING_VDB_NAME);
        }

        UnitOfWork uow = null;
        try {
            uow = createTransaction(principal, "removeVdbFromWorkspace", false); //$NON-NLS-1$

            final WorkspaceManager mgr = getWorkspaceManager();
            Vdb vdb = mgr.findVdb(vdbName);

            if (vdb == null)
                return Response.noContent().build();

            mgr.deleteVdb(vdb);

            KomodoStatusObject kso = new KomodoStatusObject("Delete Status"); //$NON-NLS-1$
            kso.addAttribute(vdbName, "Successfully deleted"); //$NON-NLS-1$

            return commit(uow, mediaTypes, kso);
        } catch (final Exception e) {
            if ((uow != null) && (uow.getState() != State.ROLLED_BACK)) {
                uow.rollback();
            }

            if (e instanceof KomodoRestException) {
                throw (KomodoRestException)e;
            }

            return createErrorResponseWithForbidden(mediaTypes, e, RelationalMessages.Error.VDB_SERVICE_DELETE_VDB_ERROR);
        }
    }

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
	 * @throws KomodoRestException
	 *         if there is a problem validating the View name or constructing
	 *         the response
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
                                      final @PathParam( "vdbName" ) String vdbName,
                                      @ApiParam(value = "Name of the Model to get its tables", required = true)
                                      final @PathParam( "modelName" ) String modelName,
                                      @ApiParam( value = "The View name being checked", required = true )
                                      final @PathParam( "viewName" ) String viewName ) throws KomodoRestException {

        SecurityPrincipal principal = checkSecurityContext(headers);
        if (principal.hasErrorResponse())
            return principal.getErrorResponse();

        final String errorMsg = VALIDATOR.checkValidName( viewName );

        // a name validation error occurred
        if ( errorMsg != null ) {
            return Response.ok().entity( errorMsg ).build();
        }

        UnitOfWork uow = null;

        try {
            uow = createTransaction( principal, "validateViewName", true ); //$NON-NLS-1$

            String[] names = RestDataservice.getViewDefnNames(getWorkspaceManager(), vdbName);
            
            for (String name : names) {
            	if (viewName.equals(name)) {
                    // name is the same as an existing View
            		return Response.ok()
                            .entity( RelationalMessages.getString( VIEW_NAME_EXISTS ) )
                            .build();
            	}
            }
            
        	// name is valid
        	return Response.ok().build();
        } catch ( final Exception e ) {
            if ( ( uow != null ) && ( uow.getState() != State.ROLLED_BACK ) ) {
                uow.rollback();
            }

            if ( e instanceof KomodoRestException ) {
                throw ( KomodoRestException )e;
            }

            return createErrorResponseWithForbidden( headers.getAcceptableMediaTypes(),
                                                     e,
                                                     VIEW_NAME_VALIDATION_ERROR );
        }
    }

}
