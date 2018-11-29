/*************************************************************************************
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
package org.komodo.relational.importer.ddl;

import java.io.File;
import java.io.InputStream;

import org.komodo.importer.AbstractImporter;
import org.komodo.importer.ImportMessages;
import org.komodo.importer.ImportOptions;
import org.komodo.importer.ImportOptions.OptionKeys;
import org.komodo.importer.ImportType;
import org.komodo.importer.Messages;
import org.komodo.relational.model.Model;
import org.komodo.relational.model.Schema;
import org.komodo.spi.KException;
import org.komodo.spi.repository.KomodoObject;
import org.komodo.spi.repository.KomodoType;
import org.komodo.spi.repository.Repository;
import org.komodo.spi.repository.Repository.UnitOfWork;
import org.komodo.utils.ArgCheck;
import org.komodo.utils.ModelType;

/**
 * Importer implementation for importing from DDL Schema.
 */
public class DdlImporter extends AbstractImporter {

    /**
     * constructor
     *
     * @param repository repository into which ddl should be imported
     */
    public DdlImporter(Repository repository) {
        // set default import type to model
        super(repository, ImportType.MODEL);
    }

    @Override
    protected void executeImport(UnitOfWork transaction, String content,
    		                             KomodoObject parentObject, ImportOptions importOptions,
    		                             ImportMessages importMessages) throws KException {

    	// Get the parentObject type that we are importing into
    	KomodoType kType = parentObject.getTypeIdentifier(transaction);

        switch(kType) {
            case MODEL:
            {
            	Model model = Model.RESOLVER.resolve(transaction, parentObject);
                ModelType.Type modelType = (ModelType.Type) importOptions.getOption(OptionKeys.MODEL_TYPE);
                model.setModelType(transaction, Model.Type.valueOf(modelType.toString()));
                model.setModelDefinition(transaction, content);
                return;
            }
            case SCHEMA:
            {
            	Schema schema = Schema.RESOLVER.resolve(transaction, parentObject);
                schema.setRendition(transaction, content);
                return;
            }
            default:
                throw new UnsupportedOperationException("DDL Import parent object should be either a model or schema"); //$NON-NLS-1$
        }
    }

    /**
	 * @throws KException
	 */
    @Override
    protected boolean handleExistingNode(UnitOfWork transaction,
                                                                             KomodoObject parentObject,
                                                                             ImportOptions importOptions,
                                                                             ImportMessages importMessages) throws KException {

    	return true;
    }

    /**
     * Perform the DDL import using the specified DDL File.  The DDL constructs must be valid to put directly beneath the parentObject.
     * @param uow the transaction
     * @param ddlFile the DDL file
     * @param parentObject the target parent object to place the new objects
     * @param importOptions the options for the import
     * @param importMessages the messages recorded during the import
     */
    public void importDdl(UnitOfWork uow, File ddlFile, KomodoObject parentObject, ImportOptions importOptions, ImportMessages importMessages) {
        if (!validFile(ddlFile, importMessages))
            return;

        try {
            doImport(uow, toString(ddlFile), parentObject, importOptions, importMessages);
        } catch (Exception ex) {
            importMessages.addErrorMessage(ex.getLocalizedMessage());
        }
    }

    /**
     * Perform the model import using the specified DDL Stream.  The DDL constructs must be valid to put directly beneath a model.
     * @param uow the transaction
     * @param ddlStream the DDL input stream
     * @param parentObject the target parent object to place the new objects
     * @param importOptions the options for the import
     * @param importMessages the messages recorded during the import
     */
    public void importDdl(UnitOfWork uow, InputStream ddlStream, KomodoObject parentObject, ImportOptions importOptions, ImportMessages importMessages) {
        ArgCheck.isNotNull(ddlStream);

        try {
            doImport(uow, toString(ddlStream), parentObject, importOptions, importMessages);
        } catch (Exception ex) {
            String errorMsg = ex.getLocalizedMessage() != null ? ex.getLocalizedMessage() : ex.getClass().getSimpleName();
            importMessages.addErrorMessage(errorMsg);
        }
    }

    /**
     * Perform the model import using the specified DDL.  The DDL constructs must be valid to put directly beneath a model.
     * @param uow the transaction
     * @param ddl the DDL
     * @param parentObject the target parent object to place the new objects
     * @param importOptions the options for the import
     * @param importMessages the messages recorded during the import
     */
    public void importDdl(UnitOfWork uow, String ddl, KomodoObject parentObject, ImportOptions importOptions, ImportMessages importMessages) {
        try {
            doImport(uow, ddl, parentObject, importOptions, importMessages);
        } catch (Exception ex) {
            importMessages.addErrorMessage(ex.getLocalizedMessage());
        }
    }

    /**
     * Set the import type. DDL importer does NOT support the VDB import type
     *
     * @param importType the type of import
     */
    public void setImportType(ImportType importType) {
        if (ImportType.VDB.equals(importType))
            throw new UnsupportedOperationException(Messages.getString(Messages.IMPORTER.ddlDoesNotSupportVDB));

        this.importType = importType;
    }
}
