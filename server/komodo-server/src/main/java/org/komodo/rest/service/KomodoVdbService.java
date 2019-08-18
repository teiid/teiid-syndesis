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

import static org.komodo.StringConstants.*;
import static org.komodo.rest.datavirtualization.RelationalMessages.Error.*;

import org.komodo.datavirtualization.ViewDefinition;
import org.komodo.rest.KomodoService;
import org.komodo.rest.V1Constants;
import org.komodo.rest.datavirtualization.RelationalMessages;
import org.komodo.utils.StringNameValidator;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.web.bind.annotation.RestController;

import io.swagger.annotations.Api;
import io.swagger.annotations.ApiOperation;
import io.swagger.annotations.ApiParam;
import io.swagger.annotations.ApiResponse;
import io.swagger.annotations.ApiResponses;
/**
 * A Komodo REST service for obtaining VDB information from the workspace.
 */
@RestController
@RequestMapping(V1Constants.APP_PATH + V1Constants.FS + V1Constants.WORKSPACE_SEGMENT
        + FS + V1Constants.VDBS_SEGMENT)
@Api(tags = {V1Constants.VDBS_SEGMENT})
public final class KomodoVdbService extends KomodoService {

    private static final StringNameValidator VALIDATOR = new StringNameValidator();

    /**
     * @param virtualization
     *        the id of the Vdb being retrieved (cannot be empty)
     * @param viewName
     *        the view name being validated (cannot be empty)
     * @return the response (never <code>null</code>) with an entity that is
     *         either an empty string, when the name is valid, or an error
     *         message
     * @throws Exception
     */
    @RequestMapping( value = V1Constants.VDB_PLACEHOLDER + FS +
                V1Constants.MODELS_SEGMENT + FS +
                V1Constants.MODEL_PLACEHOLDER + FS +
                V1Constants.VIEWS_SEGMENT + FS +
                V1Constants.NAME_VALIDATION_SEGMENT + FS + V1Constants.VIEW_PLACEHOLDER,
                method = RequestMethod.GET, produces= { MediaType.TEXT_PLAIN_VALUE })
    @ApiOperation( value = "Returns an error message if the view name is invalid" )
    @ApiResponses( value = {
            @ApiResponse( code = 400, message = "The URI cannot contain encoded slashes or backslashes." ),
            @ApiResponse( code = 403, message = "An unexpected error has occurred." ),
            @ApiResponse( code = 404, message = "No vdb could be found with name" ),
            @ApiResponse( code = 500, message = "The view name cannot be empty." )
    } )
    public ResponseEntity<String> validateViewName(
                                      @ApiParam(value = "Name of the Vdb", required = true)
                                      final @PathVariable( "virtualization" ) String virtualization,
                                      @ApiParam(value = "Name of the Model to get its tables", required = true)
                                      final @PathVariable( "viewName" ) String viewName ) throws Exception {

        String principal = checkSecurityContext();

        final String errorMsg = VALIDATOR.checkValidName( viewName );

        // a name validation error occurred
        if ( errorMsg != null ) {
            return ResponseEntity.ok(errorMsg);
        }

        return runInTransaction(principal, "validateViewName", true, ()-> { //$NON-NLS-1$
            ViewDefinition vd = getWorkspaceManager().findViewDefinitionByNameIgnoreCase(virtualization, viewName);

            if (vd != null) {
                // name is the same as an existing View
                return ResponseEntity.ok(RelationalMessages.getString(VIEW_NAME_EXISTS));
            }

            // name is valid
            return ResponseEntity.ok().build();
        }) ;
    }

}