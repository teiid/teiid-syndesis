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

import org.komodo.rest.relational.dataservice.RestDataservice;
import org.komodo.rest.relational.response.RestServiceCatalogDataSource;
import org.komodo.spi.repository.KomodoType;

import io.swagger.converter.ModelConverterContext;
import io.swagger.models.ModelImpl;

/**
 * Converter to display properties of {@link RestDataservice} class in swagger
 */
public class RestServiceCatalogDataSourceConverter extends RestEntityConverter<RestServiceCatalogDataSource> {

    @Override
    protected Class<RestServiceCatalogDataSource> getEntityClass() {
        return RestServiceCatalogDataSource.class;
    }

    @Override
    protected KomodoType getKomodoType() {
        return KomodoType.SERVICE_CATALOG_DATA_SOURCE;
    }

    @Override
    protected void addProperties(ModelImpl model, ModelConverterContext context) throws Exception {
        model.property(RestServiceCatalogDataSource.NAME_LABEL, requiredProperty(String.class));
        model.property(RestServiceCatalogDataSource.TYPE_LABEL, property(String.class));
        model.property(RestServiceCatalogDataSource.BOUND_LABEL, property(Boolean.class));
        model.property(RestServiceCatalogDataSource.TRANSLATOR_LABEL, property(String.class));
    }
}
