/*************************************************************************************
 * JBoss, Home of Professional Open Source.
 * See the COPYRIGHT.txt file distributed with this work for information
 * regarding copyright ownership. Some portions may be licensed
 * to Red Hat, Inc. under one or more contributor license agreements.
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2.1 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
 * 02110-1301 USA.
 ************************************************************************************/
package org.komodo.relational.importer.ddl;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertTrue;
import java.io.File;
import java.io.InputStream;
import java.nio.file.Files;
import java.nio.file.StandardCopyOption;
import java.util.List;
import org.junit.Ignore;
import org.junit.Test;
import org.komodo.core.KomodoLexicon;
import org.komodo.core.repository.SynchronousCallback;
import org.komodo.importer.ImportMessages;
import org.komodo.importer.ImportOptions;
import org.komodo.relational.AbstractImporterTest;
import org.komodo.relational.model.Model;
import org.komodo.relational.model.Schema;
import org.komodo.relational.vdb.Vdb;
import org.komodo.relational.workspace.WorkspaceManager;
import org.komodo.spi.lexicon.ddl.StandardDdlLexicon;
import org.komodo.spi.lexicon.ddl.teiid.TeiidDdlLexicon;
import org.komodo.spi.lexicon.vdb.VdbLexicon;
import org.komodo.spi.repository.KomodoObject;
import org.komodo.spi.repository.Repository;
import org.komodo.spi.repository.Repository.UnitOfWork.State;
import org.komodo.test.utils.TestUtilities;

/**
 * Test Class to test Teiid DDL import
 *
 */
@SuppressWarnings({"nls", "javadoc"})
public class TestTeiidDdlImporter extends AbstractImporterTest {

	private static final String TEIID_MYSQL_ACCTS = "Teiid-MySQLAccounts.ddl";

	private static final String INVALID_KEYWORD_DDL = "invalid-keyword.ddl";

	private static final String INVALID_COLUMN_DDL = "invalid-column.ddl";

	private static final String TEIID_FLATFILE = "Teiid-FlatFile.ddl";

	private static final String MODEL_NAME = "MyModel";
	private static final String SCHEMA_NAME = "MySchema";
	private static final String VDB_NAME = "MyVDB";

	@Override
	protected void runImporter(Repository repository,
	                                                             File file, KomodoObject parentObject, ImportOptions importOptions,
	                                                             ImportMessages importMessages) throws Exception {
        DdlImporter importer = new DdlImporter(_repo);
        importer.importDdl(getTransaction(), file, parentObject, importOptions, importMessages);
	}

	@Override
	protected void runImporter(Repository repository,
	                                                             InputStream inputStream, KomodoObject parentObject,
	                                                             ImportOptions importOptions,
	                                                             ImportMessages importMessages) throws Exception {
	    DdlImporter importer = new DdlImporter(_repo);
        importer.importDdl(getTransaction(), inputStream, parentObject, importOptions, importMessages);
	}

	// Commit Transaction and handle Importer errors, adding to import messages.  Then start a new transaction.
	private void commitHandleErrors(ImportMessages importMessages, State expectedState) throws Exception {
	    // cache current callback as a new one will be created when the commit occurs
	    final SynchronousCallback testCallback = this.callback;

	    // Commit the transaction and handle any import exceptions
    	commit(expectedState);

        if ( testCallback.hasError() ) {
            importMessages.addErrorMessage( testCallback.error() );
        }
	}

	// Commit Transaction and handle Importer errors, adding to import messages.  Then start a new transaction.
	private void commitHandleErrors(ImportMessages importMessages) throws Exception {
		commitHandleErrors(importMessages, State.COMMITTED);
	}

	/**
     * Test Error condition - bad DDL file name supplied
     * Expected Outcome - Error message saying that the supplied file is not found
     */
    @Test
    public void testBadDdlFile() throws Exception {
    	ImportMessages importMessages = new ImportMessages();
        KomodoObject workspace = _repo.komodoWorkspace(getTransaction());
    	executeImporter(new File("unknown.ddl"), workspace, new ImportOptions(), importMessages);

        // Verify no children created
    	KomodoObject[] children = workspace.getChildren(getTransaction());
    	assertEquals(0,children.length);

    	// Should have 1 error message
    	assertEquals(1, importMessages.getErrorMessages().size());

    	String msg = importMessages.getErrorMessages().get(0);
    	assertEquals("The specified File \"unknown.ddl\" was not found",msg);
    }

	/**
     * Test Error condition - unreadable DDL file supplied.
     * Expected Outcome - Error Message saying that the supplied file is not readable
     */
    @Test
    public void testUnreadableDdlFile() throws Exception {
        InputStream ddlStream = TestUtilities.getResourceAsStream(getClass(),
                                                                  DDL_DIRECTORY, TEIID_MYSQL_ACCTS);

    	File tmpFile = File.createTempFile("unreadableFile", ".ddl");
    	Files.copy(ddlStream, tmpFile.toPath(), StandardCopyOption.REPLACE_EXISTING);
    	tmpFile.deleteOnExit();

    	assertTrue(tmpFile.exists());
    	assertTrue(tmpFile.length() > 0);

        // Make file unreadable
        if (! tmpFile.setReadable(false)) {
            //
            // Cannot set the permissions on the file.
            // Some platforms do not allow this, eg. Windows
            // Abort the test
            //
            return;
        }

    	// Saves Messages during import
    	ImportMessages importMessages = new ImportMessages();

        KomodoObject workspace = _repo.komodoWorkspace(getTransaction());
    	executeImporter(tmpFile, workspace, new ImportOptions(), importMessages);

    	// Set back to readable
    	tmpFile.setReadable(true);

        // Verify no children created
    	KomodoObject[] children = workspace.getChildren(getTransaction());
    	assertEquals(0,children.length);

    	// Should have 1 error message
    	assertEquals(1, importMessages.getErrorMessages().size());

    	String msg = importMessages.getErrorMessages().get(0);
    	assertEquals("The specified File \"" + tmpFile.getName() + "\" is not readable", msg);
    }

    /**
     * Test Error condition - empty DDL string supplied
     * Expected Outcome - Error Message saying that the supplied DDL string is empty
     */
    @Test
    public void testEmptyDdlString() throws Exception {
        File tmpFile = File.createTempFile("emptyFile", ".ddl");
        tmpFile.deleteOnExit();

        assertTrue(tmpFile.exists());
        assertEquals(0, tmpFile.length());

        // Saves Messages during import
        ImportMessages importMessages = new ImportMessages();

        KomodoObject workspace = _repo.komodoWorkspace(getTransaction());
        executeImporter(tmpFile, workspace, new ImportOptions(), importMessages);

        // Verify no children created
    	KomodoObject[] children = workspace.getChildren(getTransaction());
    	assertEquals(0,children.length);

        // Should have 1 error message
        assertEquals(1, importMessages.getErrorMessages().size());

        String msg = importMessages.getErrorMessages().get(0);
        assertEquals("The supplied content string is empty", msg);
    }

    // Verifies a MySQL model node
    private void verifyMySQLAcctsDdl(KomodoObject modelNode) throws Exception {
        // ----------------------------------
        // Test expected tables exist
        // ----------------------------------
        KomodoObject accountTableNode = verify(getTransaction(), modelNode, "accounts.ACCOUNT", TeiidDdlLexicon.CreateTable.TABLE_STATEMENT);
        KomodoObject customerTableNode = verify(getTransaction(), modelNode, "accounts.CUSTOMER", TeiidDdlLexicon.CreateTable.TABLE_STATEMENT);
        KomodoObject holdingsTableNode = verify(getTransaction(), modelNode, "accounts.HOLDINGS", TeiidDdlLexicon.CreateTable.TABLE_STATEMENT);
        KomodoObject productTableNode = verify(getTransaction(), modelNode, "accounts.PRODUCT", TeiidDdlLexicon.CreateTable.TABLE_STATEMENT);
        KomodoObject subsTableNode = verify(getTransaction(), modelNode, "accounts.SUBSCRIPTIONS", TeiidDdlLexicon.CreateTable.TABLE_STATEMENT);

        // ----------------------------------------
        // Test expected columns for ACCOUNT table
        // ----------------------------------------
        verify(getTransaction(), accountTableNode, "ACCOUNT_ID", TeiidDdlLexicon.CreateTable.TABLE_ELEMENT);
        verify(getTransaction(), accountTableNode, "SSN", TeiidDdlLexicon.CreateTable.TABLE_ELEMENT);
        verify(getTransaction(), accountTableNode, "STATUS", TeiidDdlLexicon.CreateTable.TABLE_ELEMENT);
        verify(getTransaction(), accountTableNode, "TYPE", TeiidDdlLexicon.CreateTable.TABLE_ELEMENT);
        verify(getTransaction(), accountTableNode, "DATEOPENED", TeiidDdlLexicon.CreateTable.TABLE_ELEMENT);
        verify(getTransaction(), accountTableNode, "DATECLOSED", TeiidDdlLexicon.CreateTable.TABLE_ELEMENT);

        // ------------------------------------------
        // Test expected columns for CUSTOMER table
        // ------------------------------------------
        verify(getTransaction(), customerTableNode, "SSN", TeiidDdlLexicon.CreateTable.TABLE_ELEMENT);
        verify(getTransaction(), customerTableNode, "FIRSTNAME", TeiidDdlLexicon.CreateTable.TABLE_ELEMENT);
        verify(getTransaction(), customerTableNode, "LASTNAME", TeiidDdlLexicon.CreateTable.TABLE_ELEMENT);
        verify(getTransaction(), customerTableNode, "ST_ADDRESS", TeiidDdlLexicon.CreateTable.TABLE_ELEMENT);
        verify(getTransaction(), customerTableNode, "APT_NUMBER", TeiidDdlLexicon.CreateTable.TABLE_ELEMENT);
        verify(getTransaction(), customerTableNode, "CITY", TeiidDdlLexicon.CreateTable.TABLE_ELEMENT);
        verify(getTransaction(), customerTableNode, "STATE", TeiidDdlLexicon.CreateTable.TABLE_ELEMENT);
        verify(getTransaction(), customerTableNode, "ZIPCODE", TeiidDdlLexicon.CreateTable.TABLE_ELEMENT);
        verify(getTransaction(), customerTableNode, "PHONE", TeiidDdlLexicon.CreateTable.TABLE_ELEMENT);

        // ------------------------------------------
        // Test expected columns for HOLDINGS table
        // ------------------------------------------
        verify(getTransaction(), holdingsTableNode, "TRANSACTION_ID", TeiidDdlLexicon.CreateTable.TABLE_ELEMENT);
        verify(getTransaction(), holdingsTableNode, "ACCOUNT_ID", TeiidDdlLexicon.CreateTable.TABLE_ELEMENT);
        verify(getTransaction(), holdingsTableNode, "PRODUCT_ID", TeiidDdlLexicon.CreateTable.TABLE_ELEMENT);
        verify(getTransaction(), holdingsTableNode, "PURCHASE_DATE", TeiidDdlLexicon.CreateTable.TABLE_ELEMENT);
        verify(getTransaction(), holdingsTableNode, "SHARES_COUNT", TeiidDdlLexicon.CreateTable.TABLE_ELEMENT);

        // ------------------------------------------
        // Test expected columns for PRODUCT table
        // ------------------------------------------
        verify(getTransaction(), productTableNode, "ID", TeiidDdlLexicon.CreateTable.TABLE_ELEMENT);
        verify(getTransaction(), productTableNode, "SYMBOL", TeiidDdlLexicon.CreateTable.TABLE_ELEMENT);
        verify(getTransaction(), productTableNode, "COMPANY_NAME", TeiidDdlLexicon.CreateTable.TABLE_ELEMENT);

        // ------------------------------------------
        // Test expected columns for SUBSCRIPTIONS table
        // ------------------------------------------
        verify(getTransaction(), subsTableNode, "value", TeiidDdlLexicon.CreateTable.TABLE_ELEMENT);
        verify(getTransaction(), subsTableNode, "type", TeiidDdlLexicon.CreateTable.TABLE_ELEMENT);
        verify(getTransaction(), subsTableNode, "end_date", TeiidDdlLexicon.CreateTable.TABLE_ELEMENT);

        // --------------------------------------------
        // Test expected properties on HOLDINGS table
        // --------------------------------------------
        // Expected properties
        KomodoObject nameInSource = verify(getTransaction(), holdingsTableNode, "NAMEINSOURCE", StandardDdlLexicon.TYPE_STATEMENT_OPTION);
        verifyProperty(getTransaction(), nameInSource, StandardDdlLexicon.VALUE, "`accounts`.`HOLDINGS`");

        KomodoObject updateable = verify(getTransaction(), holdingsTableNode, "UPDATABLE", StandardDdlLexicon.TYPE_STATEMENT_OPTION);
        verifyProperty(getTransaction(), updateable, StandardDdlLexicon.VALUE, "TRUE");

        // -------------------------------------------------------------
        // Test expected properties on HOLDINGS.PURCHASE_DATE column
        // -------------------------------------------------------------
        KomodoObject purcharseDateNode = verify(getTransaction(), holdingsTableNode, "PURCHASE_DATE", TeiidDdlLexicon.CreateTable.TABLE_ELEMENT);

        // Expected properties
        KomodoObject nativeType = verify(getTransaction(), purcharseDateNode, "NATIVE_TYPE", StandardDdlLexicon.TYPE_STATEMENT_OPTION);
        verifyProperty(getTransaction(), nativeType, StandardDdlLexicon.VALUE, "TIMESTAMP");

        nameInSource = verify(getTransaction(), purcharseDateNode, "NAMEINSOURCE", StandardDdlLexicon.TYPE_STATEMENT_OPTION);
        verifyProperty(getTransaction(), nameInSource, StandardDdlLexicon.VALUE, "`PURCHASE_DATE`");

        verifyProperty(getTransaction(), purcharseDateNode, StandardDdlLexicon.DEFAULT_VALUE, "CURRENT_TIMESTAMP");
        verifyProperty(getTransaction(), purcharseDateNode, StandardDdlLexicon.NULLABLE, "NOT NULL");
        verifyProperty(getTransaction(), purcharseDateNode, StandardDdlLexicon.DEFAULT_OPTION, "DATETIME");
    }

	/**
     * Test import of Teiid-MySQLAccounts.ddl
     * Expected outcome - successful creation
     */
    @Test
    public void testDdlImport_MySQLAcctsAsModel() throws Exception {
        InputStream ddlStream = TestUtilities.getResourceAsStream(getClass(),
                                                                  DDL_DIRECTORY, TEIID_MYSQL_ACCTS);

        // Create a VDB and Model that the content will be put under
        Vdb vdb = WorkspaceManager.getInstance(_repo, getTransaction()).createVdb(getTransaction(), null, VDB_NAME, "externalPath");
        Model model = WorkspaceManager.getInstance(_repo, getTransaction()).createModel(getTransaction(), vdb, MODEL_NAME);

    	ImportOptions importOptions = new ImportOptions();
        ImportMessages importMessages = new ImportMessages();
    	executeImporter(ddlStream,model,importOptions,importMessages);

    	// Commit the transaction and handle any import exceptions
    	commitHandleErrors(importMessages);

    	// Retrive model after import
    	assertEquals(1, vdb.getModels(getTransaction()).length);
    	Model resultModel = vdb.getModels(getTransaction())[0];

    	// Test Model name
    	String modelName = resultModel.getName(getTransaction());
    	assertEquals(MODEL_NAME, modelName);

    	verifyMySQLAcctsDdl(resultModel);
    }

    @Test
    @Ignore("Test inexplicably failing randomly")
    public void testDdlImport_MySQLAcctsAsSchema() throws Exception {
        InputStream ddlStream = TestUtilities.getResourceAsStream(getClass(),
                                                                  DDL_DIRECTORY, TEIID_MYSQL_ACCTS);

        // Create a Schema that the content will be put under
        Schema schema = WorkspaceManager.getInstance(_repo, getTransaction()).createSchema(getTransaction(), null, SCHEMA_NAME);

        ImportOptions importOptions = new ImportOptions();
        ImportMessages importMessages = new ImportMessages();
        executeImporter(ddlStream,schema,importOptions,importMessages);

    	// Commit the transaction and handle any import exceptions
    	commitHandleErrors(importMessages);

    	// Retrive schema after import
    	assertEquals(1, WorkspaceManager.getInstance(_repo, getTransaction()).findSchemas(getTransaction()).length);
    	Schema schemaNode = WorkspaceManager.getInstance(_repo, getTransaction()).findSchemas(getTransaction())[0];

        // Test that a Schema was created
        assertNotNull("Failed - No Schema fragment Created ", schemaNode);
        verifyPrimaryType(getTransaction(), schemaNode, KomodoLexicon.Schema.NODE_TYPE);

        // Test Schema name
        String schemaName = schemaNode.getName(getTransaction());
        assertEquals(SCHEMA_NAME, schemaName);

        verifyMySQLAcctsDdl(schemaNode);
    }

	/**
     * Imports MySQL Model DDL, then re-imports.  import of DDL into a parent does a full replace of the existing content...
     * Expected outcome - successful creation with replacement of first import content
     */
    @Test
    public void testDdlImportModelThenReimport() throws Exception {
        // Import the original model from DDL
    	testDdlImport_MySQLAcctsAsModel();
        commit();

        // Now re-import the FlatFile DDL
        InputStream ddlStream = TestUtilities.getResourceAsStream(getClass(), DDL_DIRECTORY, TEIID_FLATFILE);

        // Get the model created in the first import
        Model model = WorkspaceManager.getInstance(_repo, getTransaction()).findModels(getTransaction())[0];

    	ImportOptions importOptions = new ImportOptions();
        ImportMessages importMessages = new ImportMessages();
    	executeImporter(ddlStream,model,importOptions,importMessages);

    	// Commit the transaction and handle any import exceptions
    	commitHandleErrors(importMessages);

    	// Retrieve model after import
        Model[] models = WorkspaceManager.getInstance(_repo, getTransaction()).findModels(getTransaction());
    	assertEquals(1, models.length);
    	Model resultModel = models[0];

    	// Test Model name
    	String modelName = resultModel.getName(getTransaction());
    	assertEquals(MODEL_NAME, modelName);

    	verifyFlatFileDdl(resultModel);
    }

    // Verifies the FlatFile node
    private void verifyFlatFileDdl(KomodoObject schemaNode) throws Exception {
        // ----------------------------------
        // Test expected procedures exist
        // ----------------------------------
        KomodoObject getFilesProcNode = verify(getTransaction(), schemaNode, "getFiles", TeiidDdlLexicon.CreateProcedure.PROCEDURE_STATEMENT);
        verify(getTransaction(), schemaNode, "getTextFiles", TeiidDdlLexicon.CreateProcedure.PROCEDURE_STATEMENT);
        verify(getTransaction(), schemaNode, "saveFile", TeiidDdlLexicon.CreateProcedure.PROCEDURE_STATEMENT);

        // --------------------------------------------
        // Test getFiles procedure has expected param
        // --------------------------------------------
        verify(getTransaction(), getFilesProcNode, "pathAndPattern", TeiidDdlLexicon.CreateProcedure.PARAMETER);

        // --------------------------------------------
        // Test getFiles procedure properties
        // --------------------------------------------
        // Expected properties
        KomodoObject description = verify(getTransaction(), getFilesProcNode, "ANNOTATION", StandardDdlLexicon.TYPE_STATEMENT_OPTION);
        verifyProperty(getTransaction(), description, StandardDdlLexicon.VALUE, "Returns files that match the given path and pattern as BLOBs");

        // ------------------------------------------------
        // Test getFiles procedure has expected resultSet
        // ------------------------------------------------
        KomodoObject resultSet = verify(getTransaction(), getFilesProcNode, "resultSet", TeiidDdlLexicon.CreateProcedure.RESULT_COLUMNS);

        // -------------------------------------------------------------
        // Test resultSet has expected columns
        // -------------------------------------------------------------
        verify(getTransaction(), resultSet, "file", TeiidDdlLexicon.CreateProcedure.RESULT_COLUMN);
        verify(getTransaction(), resultSet, "filePath", TeiidDdlLexicon.CreateProcedure.RESULT_COLUMN);
    }

	/**
     * Test import of Teiid-FlatFile.ddl
     * Expected outcome - successful creation
     */
    @Test
    public void testDdlImport_FlatFileAsModel() throws Exception {
        InputStream ddlStream = TestUtilities.getResourceAsStream(getClass(),
                                                                  DDL_DIRECTORY, TEIID_FLATFILE);

        // Create a VDB and Model that the content will be put under
        Vdb vdb = WorkspaceManager.getInstance(_repo, getTransaction()).createVdb(getTransaction(), null, VDB_NAME, "externalPath");
        Model model = WorkspaceManager.getInstance(_repo, getTransaction()).createModel(getTransaction(), vdb, MODEL_NAME);

        ImportOptions importOptions = new ImportOptions();
        ImportMessages importMessages = new ImportMessages();
    	executeImporter(ddlStream,model,importOptions,importMessages);

    	// Commit the transaction and handle any import exceptions
    	commitHandleErrors(importMessages);

    	// Retrieve model after import
    	assertEquals(1, vdb.getModels(getTransaction()).length);
    	Model modelNode = vdb.getModels(getTransaction())[0];

    	// Test that a Model was created
    	assertNotNull("Failed - No Model Created ", modelNode);
    	verifyPrimaryType(getTransaction(), modelNode, VdbLexicon.Vdb.DECLARATIVE_MODEL);

    	// Test Model name
    	String modelName = modelNode.getName(getTransaction());
    	assertEquals(MODEL_NAME, modelName);

        verifyFlatFileDdl(modelNode);
    }

    @Test
    @Ignore("Test is inexplicably failing when executed along with the other tests")
    public void testDdlImport_FlatFileAsSchema() throws Exception {
        InputStream ddlStream = TestUtilities.getResourceAsStream(getClass(),
                                                                  DDL_DIRECTORY, TEIID_FLATFILE);

        // Create a Schema that the content will be put under
        Schema schema = WorkspaceManager.getInstance(_repo, getTransaction()).createSchema(getTransaction(), null, SCHEMA_NAME);

        ImportOptions importOptions = new ImportOptions();
        ImportMessages importMessages = new ImportMessages();
        executeImporter(ddlStream,schema,importOptions,importMessages);

    	// Commit the transaction and handle any import exceptions
    	commitHandleErrors(importMessages);

    	// Retrive schema after import
    	assertEquals(1, WorkspaceManager.getInstance(_repo, getTransaction()).findSchemas(getTransaction()).length);
    	Schema schemaNode = WorkspaceManager.getInstance(_repo, getTransaction()).findSchemas(getTransaction())[0];

        // Test that a Model was created
        assertNotNull("Failed - No Schema fragment Created ", schemaNode);
        verifyPrimaryType(getTransaction(), schemaNode, KomodoLexicon.Schema.NODE_TYPE);

        // Test Schema name
        String schemaName = schemaNode.getName(getTransaction());
        assertEquals(SCHEMA_NAME, schemaName);

        verifyFlatFileDdl(schemaNode);
    }

    @Test
    public void testInvalidKeywordDdlSequencerError() throws Exception {
        InputStream ddlStream =  TestUtilities.getResourceAsStream(getClass(),
                                                                   DDL_DIRECTORY,
                                                                   INVALID_KEYWORD_DDL);

        // Create a VDB and Model that the content will be put under
        Vdb vdb = WorkspaceManager.getInstance(_repo, getTransaction()).createVdb(getTransaction(), null, VDB_NAME, "externalPath");
        Model model = WorkspaceManager.getInstance(_repo, getTransaction()).createModel(getTransaction(), vdb, MODEL_NAME);

        ImportOptions importOptions = new ImportOptions();
        ImportMessages importMessages = new ImportMessages();
        executeImporter(ddlStream,model,importOptions,importMessages);

    	// Commit the transaction and handle any import exceptions
    	commitHandleErrors(importMessages, State.ERROR);

    	// Retrive model after import
    	assertEquals(1, vdb.getModels(getTransaction()).length);
    	Model modelNode = vdb.getModels(getTransaction())[0];
    	assertEquals(MODEL_NAME,modelNode.getName(getTransaction()));

        // Error messages - expects one parsing error
        List<String> errorMessages = importMessages.getErrorMessages();
        assertEquals(1, errorMessages.size());

        String expErrorMsg = "DDL Parsing encountered unknown statement:" + NEW_LINE +
                                           "CREATE INVALID TABLE \"accounts.ACCOUNT\" (" + NEW_LINE +
                                           TAB + "INVALID_ID long CAN BE NULL DEFAULT '0'" + NEW_LINE +
                                           ");";
        assertEquals(expErrorMsg, errorMessages.get(0));
    }

    @Test
    public void testInvalidColumnDdlSequencerError() throws Exception {
        InputStream ddlStream =  TestUtilities.getResourceAsStream(getClass(),
                                                                   DDL_DIRECTORY,
                                                                   INVALID_COLUMN_DDL);

        // Create a VDB and Model that the content will be put under
        Vdb vdb = WorkspaceManager.getInstance(_repo, getTransaction()).createVdb(getTransaction(), null, VDB_NAME, "externalPath");
        Model model = WorkspaceManager.getInstance(_repo, getTransaction()).createModel(getTransaction(), vdb, MODEL_NAME);

        ImportOptions importOptions = new ImportOptions();
        ImportMessages importMessages = new ImportMessages();
        executeImporter(ddlStream,model,importOptions,importMessages);

    	// Commit the transaction and handle any import exceptions
    	commitHandleErrors(importMessages, State.ERROR);

    	// Retrive model after import
    	assertEquals(1, vdb.getModels(getTransaction()).length);
    	Model modelNode = vdb.getModels(getTransaction())[0];
    	assertEquals(MODEL_NAME,modelNode.getName(getTransaction()));

        // Error messages - expects one parsing error
        List<String> errorMessages = importMessages.getErrorMessages();
        assertEquals(1, errorMessages.size());

        String expErrorMsg = "Unparsable table body (position = 59:2:18";
        assertEquals(expErrorMsg, errorMessages.get(0));
    }
}
