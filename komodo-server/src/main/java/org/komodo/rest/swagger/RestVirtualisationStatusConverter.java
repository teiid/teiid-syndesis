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
package org.komodo.rest.swagger;

import org.komodo.rest.relational.response.virtualization.RestVirtualizationStatus;
import org.komodo.spi.repository.KomodoType;

import io.swagger.converter.ModelConverterContext;
import io.swagger.models.ModelImpl;

/**
 * Converter to display properties of {@link RestVirtualizationStatus} class in swagger
 */
public class RestVirtualisationStatusConverter extends RestEntityConverter<RestVirtualizationStatus> {

    @Override
    protected Class<RestVirtualizationStatus> getEntityClass() {
        return RestVirtualizationStatus.class;
    }

    @Override
    protected KomodoType getKomodoType() {
        return KomodoType.BUILD_STATUS;
    }

    @Override
    protected void addProperties(ModelImpl model, ModelConverterContext context) throws Exception {
        model.property(RestVirtualizationStatus.VDB_NAME_LABEL, requiredProperty(String.class));
        model.property(RestVirtualizationStatus.BUILD_NAME_LABEL, property(String.class));
        model.property(RestVirtualizationStatus.DEPLOYMENT_NAME_LABEL, property(String.class));
        model.property(RestVirtualizationStatus.NAMESPACE_LABEL, property(String.class));
        model.property(RestVirtualizationStatus.STATUS_LABEL, property(String.class));
        model.property(RestVirtualizationStatus.STATUS_MSG_LABEL, property(String.class));
        model.property(RestVirtualizationStatus.LAST_UPDATED_LABEL, property(String.class));

    }
}
