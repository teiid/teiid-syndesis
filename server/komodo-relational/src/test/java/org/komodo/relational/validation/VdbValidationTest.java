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
package org.komodo.relational.validation;

import static org.hamcrest.core.Is.is;
import static org.hamcrest.core.IsCollectionContaining.hasItems;
import static org.junit.Assert.assertThat;

import org.junit.Test;
import org.komodo.relational.model.Column;
import org.komodo.relational.model.ForeignKey;
import org.komodo.relational.model.Model;
import org.komodo.relational.model.PrimaryKey;
import org.komodo.relational.model.SchemaElement;
import org.komodo.relational.model.Table;
import org.komodo.relational.model.UniqueConstraint;
import org.komodo.relational.model.View;
import org.komodo.relational.vdb.ModelSource;
import org.komodo.relational.vdb.Translator;
import org.komodo.relational.vdb.Vdb;
import org.komodo.relational.vdb.VdbImport;
import org.komodo.spi.outcome.Outcome.Level;
import org.komodo.spi.repository.validation.Result;
import org.komodo.spi.repository.validation.Rule;

@SuppressWarnings( { "javadoc", "nls" } )
public final class VdbValidationTest extends RelationalValidationTest {

    @Test
    public void shouldGetAllRules() throws Exception {
        final Rule[] rules = _repo.getValidationManager().getAllRules(getTransaction());
        assertThat( rules.length, is( 42 ) );
    }

    // ==============================================================================================
    // Check valid rule counts for each type
    // ==============================================================================================

    @Test
    public void shouldGetValidRulesForVdb() throws Exception {
        Vdb vdb = createVdb("myVDB");

        final Rule[] rules = _repo.getValidationManager().getRules(getTransaction(),vdb);
        assertThat( rules.length, is( 4 ) );
        assertThat( getRuleNames( rules ), hasItems( "default.vdb.nodeName",
                                                     "default.vdb.name.propertyValue",
                                                     "default.vdb.version.propertyValue",
                                                     "default.vdb.connectionType.propertyValue" ) );
    }

    @Test
    public void shouldGetValidRulesForTranslator() throws Exception {
        Vdb vdb = createVdb("myVDB");
        Translator translator = addTranslator(vdb,"myTranslator","transType");

        final Rule[] rules = _repo.getValidationManager().getRules(getTransaction(),translator);
        assertThat( rules.length, is( 1 ) );
        assertThat( getRuleNames( rules ), hasItems( "default.translator.nodeName" ) );
    }

    @Test
    public void shouldGetValidRulesForVdbImport() throws Exception {
        Vdb vdb = createVdb("myVDB");
        VdbImport vdbImport = addVdbImport(vdb,"myImport");

        final Rule[] rules = _repo.getValidationManager().getRules(getTransaction(),vdbImport);
        assertThat( rules.length, is( 1 ) );
        assertThat( getRuleNames( rules ), hasItems( "default.importVdb.nodeName" ) );
    }

    @Test
    public void shouldGetValidRulesForModel() throws Exception {
        Vdb vdb = createVdb("myVDB");
        Model model = addModel(vdb,"myModel");

        final Rule[] rules = _repo.getValidationManager().getRules(getTransaction(),model);
        assertThat( rules.length, is( 1 ) );
        assertThat( getRuleNames( rules ), hasItems( "default.declarativeModel.nodeName" ) );
    }

    @Test
    public void shouldGetValidRulesForTable() throws Exception {
        Vdb vdb = createVdb("myVDB");
        Model model = addModel(vdb,"myModel");
        Table table = addTable(model,"myTable");

        final Rule[] rules = _repo.getValidationManager().getRules(getTransaction(),table);
        assertThat( rules.length, is( 4 ) );
        assertThat( getRuleNames( rules ), hasItems( "default.table.nodeName",
                                                     "default.table.column.childSNSValidation",
                                                     "default.table.column.childCount",
                                                     "default.table.nameInSource.propertyValue" ) );
    }

    @Test
    public void shouldGetValidRulesForView() throws Exception {
        Vdb vdb = createVdb("myVDB");
        Model model = addModel(vdb,"myModel");
        View view = addView(model,"myView");

        final Rule[] rules = _repo.getValidationManager().getRules(getTransaction(),view);
        assertThat( rules.length, is( 3 ) );
        assertThat( getRuleNames( rules ), hasItems( "default.view.nodeName",
                                                     "default.view.column.childSNSValidation",
                                                     "default.view.column.childCount" ) );
    }

    @Test
    public void shouldGetValidRulesForModelSource() throws Exception {
        Vdb vdb = createVdb("myVDB");
        Model model = addModel(vdb,"myModel");
        ModelSource modelSource = addSource(model,"mySource");

        final Rule[] rules = _repo.getValidationManager().getRules(getTransaction(),modelSource);
        assertThat( rules.length, is( 1 ) );
        assertThat( getRuleNames( rules ), hasItems( "default.modelSource.nodeName" ) );
    }

    @Test
    public void shouldGetValidRulesForPrimaryKey() throws Exception {
        Vdb vdb = createVdb("myVDB");
        Model model = addModel(vdb,"myModel");
        Table table = addTable(model,"myTable");
        PrimaryKey pk = addPrimaryKey(table,"myPK");

        final Rule[] rules = _repo.getValidationManager().getRules(getTransaction(),pk);
        assertThat( rules.length, is( 1 ) );
        assertThat( getRuleNames( rules ), hasItems( "default.primaryKey.nodeName" ) );
    }

    @Test
    public void shouldGetValidRulesForForeignKey() throws Exception {
        Vdb vdb = createVdb("myVDB");
        Model model = addModel(vdb,"myModel");
        Table table = addTable(model,"myTable");
        Table refTable = addTable(model,"refTable");
        ForeignKey fk = addForeignKey(table,"myFK",refTable);

        final Rule[] rules = _repo.getValidationManager().getRules(getTransaction(),fk);
        assertThat( rules.length, is( 1 ) );
        assertThat( getRuleNames( rules ), hasItems( "default.foreignKey.nodeName" ) );
    }

    @Test
    public void shouldGetValidRulesForUniqueConstraint() throws Exception {
        Vdb vdb = createVdb("myVDB");
        Model model = addModel(vdb,"myModel");
        Table table = addTable(model,"myTable");
        UniqueConstraint uc = addUniqueConstraint(table,"myUC");

        final Rule[] rules = _repo.getValidationManager().getRules(getTransaction(),uc);
        assertThat( rules.length, is( 1 ) );
        assertThat( getRuleNames( rules ), hasItems( "default.uniqueConstraint.nodeName" ) );
    }

    @Test
    public void shouldGetValidRulesForColumn() throws Exception {
        Vdb vdb = createVdb("myVDB");
        Model model = addModel(vdb,"myModel");
        Table table = addTable(model,"myTable");
        Column col = addColumn(table,"myColumn");

        final Rule[] rules = _repo.getValidationManager().getRules(getTransaction(),col);
        assertThat( rules.length, is( 2 ) );
        assertThat( getRuleNames( rules ), hasItems( "default.column.nodeName",
                                                     "default.column.datatype.propertyValue" ) );
    }

    @Test
    public void shouldGetValidRulesForStringColumn() throws Exception {
        Vdb vdb = createVdb("myVDB");
        Model model = addModel(vdb,"myModel");
        Table table = addTable(model,"myTable");
        Column col = addColumn(table,"myColumn");
        col.setDatatypeName(getTransaction(), "string");

        final Rule[] rules = _repo.getValidationManager().getRules(getTransaction(),col);
        assertThat( rules.length, is( 3 ) );
        assertThat( getRuleNames( rules ), hasItems( "default.column.nodeName",
                                                     "default.column.datatype.propertyValue",
                                                     "default.column.stringDatatype.lengthPropertyValue" ) );
    }

    @Test
    public void shouldGetValidRulesForNumericColumn() throws Exception {
        Vdb vdb = createVdb("myVDB");
        Model model = addModel(vdb,"myModel");
        Table table = addTable(model,"myTable");
        Column col = addColumn(table,"myColumn");
        col.setDatatypeName(getTransaction(), "decimal");

        final Rule[] rules = _repo.getValidationManager().getRules(getTransaction(),col);
        assertThat( rules.length, is( 3 ) );
        assertThat( getRuleNames( rules ), hasItems( "default.column.nodeName",
                                                     "default.column.datatype.propertyValue",
                                                     "default.column.numericDatatype.precisionPropertyValue" ) );
    }

    @Test
    public void shouldTestGetVdbNodeNameRuleById() throws Exception {
        final Rule rule = _repo.getValidationManager().getRule(getTransaction(), "default.vdb.nodeName");
        assertThat( rule.getName(getTransaction()), is( "default.vdb.nodeName" ) );
    }

    @Test
    public void shouldTestGetVdbConnectionTypePropertyRuleById() throws Exception {
        final Rule rule = _repo.getValidationManager().getRule(getTransaction(), "default.vdb.connectionType.propertyValue");
        assertThat( rule.getName(getTransaction()), is( "default.vdb.connectionType.propertyValue" ) );
    }

    @Test
    public void shouldTestGetRuleByIdFailure() throws Exception {
        final Rule rule = _repo.getValidationManager().getRule(getTransaction(), "default.vdb.connectionType.badOne");
        assertThat( rule.getName(getTransaction()), is( "default.ruleNotFound" ));
        assertThat( rule.getDescription(getTransaction()), is( "Rule \"default.vdb.connectionType.badOne\" was not found." ));
    }

    // ==============================================================================================
    // VDB Validation Rules
    // ==============================================================================================

    @Test
    public void shouldTestVdbNodeNameValidationSuccess() throws Exception {
        Vdb vdb = createVdb("myVDB");

        final Result[] results = _repo.getValidationManager().evaluate(getTransaction(), vdb, "default.vdb.nodeName");
        assertThat( results.length, is( 1 ) );
        assertThat( results[0].getRuleId(), is( "default.vdb.nodeName" ) );
        assertThat( results[0].isOK(), is( true ));
    }

    @Test
    public void shouldTestVdbNodeNameValidationFailure() throws Exception {
        Vdb vdb = createVdb("1myVDB");

        final Result[] results = _repo.getValidationManager().evaluate(getTransaction(), vdb, "default.vdb.nodeName");
        assertThat( results.length, is( 1 ) );
        assertThat( results[0].getRuleId(), is( "default.vdb.nodeName" ) );
        assertThat( results[0].isOK(), is( false ));
        assertThat( results[0].getLevel(), is( Level.ERROR));
    }

    @Test
    public void shouldTestVdbNameValidationSuccess() throws Exception {
        Vdb vdb = createVdb("myVDB");
        vdb.setVdbName(getTransaction(), "vdbName");

        final Result[] results = _repo.getValidationManager().evaluate(getTransaction(), vdb, "default.vdb.name.propertyValue");
        assertThat( results.length, is( 1 ) );
        assertThat( results[0].getRuleId(), is( "default.vdb.name.propertyValue" ) );
        assertThat( results[0].isOK(), is( true ));
    }

    @Test
    public void shouldTestVdbNameValidationFailure() throws Exception {
        Vdb vdb = createVdb("myVDB");
        vdb.setVdbName(getTransaction(), "1vdbName");

        final Result[] results = _repo.getValidationManager().evaluate(getTransaction(), vdb, "default.vdb.name.propertyValue");
        assertThat( results.length, is( 1 ) );
        assertThat( results[0].getRuleId(), is( "default.vdb.name.propertyValue" ) );
        assertThat( results[0].isOK(), is( false ));
        assertThat( results[0].getLevel(), is( Level.ERROR));
    }

    @Test
    public void shouldTestVdbVersionValidationSuccess() throws Exception {
        Vdb vdb = createVdb("myVDB");
        vdb.setVersion(getTransaction(), 3);

        final Result[] results = _repo.getValidationManager().evaluate(getTransaction(), vdb, "default.vdb.version.propertyValue");
        assertThat( results.length, is( 1 ) );
        assertThat( results[0].getRuleId(), is( "default.vdb.version.propertyValue" ) );
        assertThat( results[0].isOK(), is( true ));
    }

    @Test
    public void shouldTestVdbConnectionTypeValidationSuccess() throws Exception {
        Vdb vdb = createVdb("myVDB");
        vdb.setConnectionType(getTransaction(), "vdbName");

        final Result[] results = _repo.getValidationManager().evaluate(getTransaction(), vdb, "default.vdb.connectionType.propertyValue");
        assertThat( results.length, is( 1 ) );
        assertThat( results[0].getRuleId(), is( "default.vdb.connectionType.propertyValue" ) );
        assertThat( results[0].isOK(), is( true ));
    }

    @Test
    public void shouldTestVdbConnectionTypeValidationFailure() throws Exception {
        Vdb vdb = createVdb("myVDB");

        final Result[] results = _repo.getValidationManager().evaluate(getTransaction(), vdb, "default.vdb.connectionType.propertyValue");
        assertThat( results.length, is( 1 ) );
        assertThat( results[0].getRuleId(), is( "default.vdb.connectionType.propertyValue" ) );
        assertThat( results[0].isOK(), is( false ));
        assertThat( results[0].getLevel(), is( Level.ERROR));
    }

    // ==============================================================================================
    // DataRole Validation Rules
    // ==============================================================================================

    

    // ==============================================================================================
    // Permission Validation Rules
    // ==============================================================================================

    

    // ==============================================================================================
    // Mask Validation Rules
    // ==============================================================================================

    

    // ==============================================================================================
    // Condition Validation Rules
    // ==============================================================================================

    

    // ==============================================================================================
    // Translator Validation Rules
    // ==============================================================================================

    @Test
    public void shouldTestTranslatorNodeNameValidationSuccess() throws Exception {
        Vdb vdb = createVdb("myVDB");
        Translator translator = addTranslator(vdb,"myTranslator","transType");

        final Result[] results = _repo.getValidationManager().evaluate(getTransaction(), translator, "default.translator.nodeName");
        assertThat( results.length, is( 1 ) );
        assertThat( results[0].getRuleId(), is( "default.translator.nodeName" ) );
        assertThat( results[0].isOK(), is( true ));
    }

    @Test
    public void shouldTestTranslatorNodeNameValidationFailure() throws Exception {
        Vdb vdb = createVdb("myVDB");
        Translator translator = addTranslator(vdb,"1myTranslator","transType");

        final Result[] results = _repo.getValidationManager().evaluate(getTransaction(), translator, "default.translator.nodeName");
        assertThat( results.length, is( 1 ) );
        assertThat( results[0].getRuleId(), is( "default.translator.nodeName" ) );
        assertThat( results[0].isOK(), is( false ));
        assertThat( results[0].getLevel(), is( Level.ERROR));
    }

    // ==============================================================================================
    // Entry Validation Rules
    // ==============================================================================================

    

    // ==============================================================================================
    // VdbImport Validation Rules
    // ==============================================================================================

    @Test
    public void shouldTestVdbImportNodeNameValidationSuccess() throws Exception {
        Vdb vdb = createVdb("myVDB");
        VdbImport vdbImport = addVdbImport(vdb,"myImport");

        final Result[] results = _repo.getValidationManager().evaluate(getTransaction(), vdbImport, "default.importVdb.nodeName");
        assertThat( results.length, is( 1 ) );
        assertThat( results[0].getRuleId(), is( "default.importVdb.nodeName" ) );
        assertThat( results[0].isOK(), is( true ));
    }

    @Test
    public void shouldTestVdbImportNodeNameValidationFailure() throws Exception {
        Vdb vdb = createVdb("myVDB");
        VdbImport vdbImport = addVdbImport(vdb,"1myImport");

        final Result[] results = _repo.getValidationManager().evaluate(getTransaction(), vdbImport, "default.importVdb.nodeName");
        assertThat( results.length, is( 1 ) );
        assertThat( results[0].getRuleId(), is( "default.importVdb.nodeName" ) );
        assertThat( results[0].isOK(), is( false ));
        assertThat( results[0].getLevel(), is( Level.ERROR));
    }

    // ==============================================================================================
    // Model Validation Rules
    // ==============================================================================================

    @Test
    public void shouldTestModelNodeNameValidationSuccess() throws Exception {
        Vdb vdb = createVdb("myVDB");
        Model model = addModel(vdb,"myModel");

        final Result[] results = _repo.getValidationManager().evaluate(getTransaction(), model, "default.declarativeModel.nodeName");
        assertThat( results.length, is( 1 ) );
        assertThat( results[0].getRuleId(), is( "default.declarativeModel.nodeName" ) );
        assertThat( results[0].isOK(), is( true ));
    }

    @Test
    public void shouldTestModelNodeNameValidationFailure() throws Exception {
        Vdb vdb = createVdb("myVDB");
        Model model = addModel(vdb,"1myModel");

        final Result[] results = _repo.getValidationManager().evaluate(getTransaction(), model, "default.declarativeModel.nodeName");
        assertThat( results.length, is( 1 ) );
        assertThat( results[0].getRuleId(), is( "default.declarativeModel.nodeName" ) );
        assertThat( results[0].isOK(), is( false ));
        assertThat( results[0].getLevel(), is( Level.ERROR));
    }

    // ==============================================================================================
    // Table Validation Rules
    // ==============================================================================================

    @Test
    public void shouldTestTableNodeNameValidationSuccess() throws Exception {
        Vdb vdb = createVdb("myVDB");
        Model model = addModel(vdb,"myModel");
        Table table = addTable(model,"myTable");

        final Result[] results = _repo.getValidationManager().evaluate(getTransaction(), table, "default.table.nodeName");
        assertThat( results.length, is( 1 ) );
        assertThat( results[0].getRuleId(), is( "default.table.nodeName" ) );
        assertThat( results[0].isOK(), is( true ));
    }

    @Test
    public void shouldTestTableNodeNameValidationFailure() throws Exception {
        Vdb vdb = createVdb("myVDB");
        Model model = addModel(vdb,"myModel");
        Table table = addTable(model,"1myTable");

        final Result[] results = _repo.getValidationManager().evaluate(getTransaction(), table, "default.table.nodeName");
        assertThat( results.length, is( 1 ) );
        assertThat( results[0].getRuleId(), is( "default.table.nodeName" ) );
        assertThat( results[0].isOK(), is( false ));
        assertThat( results[0].getLevel(), is( Level.ERROR));
    }

    @Test
    public void shouldTestTableColumnCountValidationSuccess() throws Exception {
        Vdb vdb = createVdb("myVDB");
        Model model = addModel(vdb,"myModel");
        Table table = addTable(model,"myTable");
        addColumn(table, "myColumn");

        final Result[] results = _repo.getValidationManager().evaluate(getTransaction(), table, "default.table.column.childCount");
        assertThat( results.length, is( 1 ) );
        assertThat( results[0].getRuleId(), is( "default.table.column.childCount" ) );
        assertThat( results[0].isOK(), is( true ));
    }

    @Test
    public void shouldTestTableColumnCountValidationFailure() throws Exception {
        Vdb vdb = createVdb("myVDB");
        Model model = addModel(vdb,"myModel");
        Table table = addTable(model,"myTable");

        final Result[] results = _repo.getValidationManager().evaluate(getTransaction(), table, "default.table.column.childCount");
        assertThat( results.length, is( 1 ) );
        assertThat( results[0].getRuleId(), is( "default.table.column.childCount" ) );
        assertThat( results[0].isOK(), is( false ));
        assertThat( results[0].getLevel(), is( Level.ERROR));
    }

    @Test
    public void shouldTestTableColumnUniqueNameValidationSuccess() throws Exception {
        Vdb vdb = createVdb("myVDB");
        Model model = addModel(vdb,"myModel");
        Table table = addTable(model,"myTable");
        addColumn(table, "myColumn1");
        addColumn(table, "myColumn2");

        final Result[] results = _repo.getValidationManager().evaluate(getTransaction(), table, "default.table.column.childSNSValidation");
        assertThat( results.length, is( 1 ) );
        assertThat( results[0].getRuleId(), is( "default.table.column.childSNSValidation" ) );
        assertThat( results[0].isOK(), is( true ));
    }

    @Test
    public void shouldTestTableColumnUniqueNameValidationFailure() throws Exception {
        Vdb vdb = createVdb("myVDB");
        Model model = addModel(vdb,"myModel");
        Table table = addTable(model,"myTable");
        addColumn(table, "myColumn1");
        addColumn(table, "myColumn1");

        final Result[] results = _repo.getValidationManager().evaluate(getTransaction(), table, "default.table.column.childSNSValidation");
        assertThat( results.length, is( 1 ) );
        assertThat( results[0].getRuleId(), is( "default.table.column.childSNSValidation" ) );
        assertThat( results[0].isOK(), is( false ));
        assertThat( results[0].getLevel(), is( Level.ERROR));
    }

    @Test
    public void shouldTestTableNameInSourceValidationSuccess() throws Exception {
        Vdb vdb = createVdb("myVDB");
        Model model = addModel(vdb,"myModel");
        Table table = addTable(model,"myTable");
        table.setNameInSource(getTransaction(), "aNIS");

        final Result[] results = _repo.getValidationManager().evaluate(getTransaction(), table, "default.table.nameInSource.propertyValue");
        assertThat( results.length, is( 1 ) );
        assertThat( results[0].getRuleId(), is( "default.table.nameInSource.propertyValue" ) );
        assertThat( results[0].isOK(), is( true ));
    }

    @Test
    public void shouldTestTableNameInSourceValidationFailure() throws Exception {
        Vdb vdb = createVdb("myVDB");
        Model model = addModel(vdb,"myModel");
        Table table = addTable(model,"myTable");

        final Result[] results = _repo.getValidationManager().evaluate(getTransaction(), table, "default.table.nameInSource.propertyValue");
        assertThat( results.length, is( 1 ) );
        assertThat( results[0].getRuleId(), is( "default.table.nameInSource.propertyValue" ) );
        assertThat( results[0].isOK(), is( false ));
        assertThat( results[0].getLevel(), is( Level.WARNING));
    }

    @Test
    public void shouldTestMaterializedTableColumnTypesValidationSuccess() throws Exception {
        Vdb vdb = createVdb("myVDB");
        Model model = addModel(vdb,"myModel");
        Table table = addTable(model,"myTable");
        table.setMaterialized(getTransaction(), true);
        table.setSchemaElementType(getTransaction(), SchemaElement.SchemaElementType.VIRTUAL);
        Column col = addColumn(table,"myColumn");
        col.setDatatypeName(getTransaction(), "string");

        final Result[] results = _repo.getValidationManager().evaluate(getTransaction(), table, "default.materializedTable.column.propertyValue");
        assertThat( results.length, is( 1 ) );
        assertThat( results[0].getRuleId(), is( "default.materializedTable.column.propertyValue" ) );
        assertThat( results[0].isOK(), is( true ));
    }

    @Test
    public void shouldTestMaterializedTableColumnTypesValidationFailure() throws Exception {
        Vdb vdb = createVdb("myVDB");
        Model model = addModel(vdb,"myModel");
        Table table = addTable(model,"myTable");
        table.setMaterialized(getTransaction(), true);
        table.setSchemaElementType(getTransaction(), SchemaElement.SchemaElementType.VIRTUAL);
        Column col = addColumn(table,"myColumn");
        col.setDatatypeName(getTransaction(), "object");

        final Result[] results = _repo.getValidationManager().evaluate(getTransaction(), table, "default.materializedTable.column.propertyValue");
        assertThat( results.length, is( 1 ) );
        assertThat( results[0].getRuleId(), is( "default.materializedTable.column.propertyValue" ) );
        assertThat( results[0].isOK(), is( false ));
        assertThat( results[0].getLevel(), is( Level.ERROR));
    }



    // ==============================================================================================
    // View Validation Rules
    // ==============================================================================================

    @Test
    public void shouldTestViewNodeNameValidationSuccess() throws Exception {
        Vdb vdb = createVdb("myVDB");
        Model model = addModel(vdb,"myModel");
        View view = addView(model,"myView");

        final Result[] results = _repo.getValidationManager().evaluate(getTransaction(), view, "default.view.nodeName");
        assertThat( results.length, is( 1 ) );
        assertThat( results[0].getRuleId(), is( "default.view.nodeName" ) );
        assertThat( results[0].isOK(), is( true ));
    }

    @Test
    public void shouldTestViewNodeNameValidationFailure() throws Exception {
        Vdb vdb = createVdb("myVDB");
        Model model = addModel(vdb,"myModel");
        View view = addView(model,"1myView");

        final Result[] results = _repo.getValidationManager().evaluate(getTransaction(), view, "default.view.nodeName");
        assertThat( results.length, is( 1 ) );
        assertThat( results[0].getRuleId(), is( "default.view.nodeName" ) );
        assertThat( results[0].isOK(), is( false ));
        assertThat( results[0].getLevel(), is( Level.ERROR));
    }

    @Test
    public void shouldTestViewColumnCountValidationSuccess() throws Exception {
        Vdb vdb = createVdb("myVDB");
        Model model = addModel(vdb,"myModel");
        View view = addView(model,"myView");
        addColumn(view, "myColumn");

        final Result[] results = _repo.getValidationManager().evaluate(getTransaction(), view, "default.view.column.childCount");
        assertThat( results.length, is( 1 ) );
        assertThat( results[0].getRuleId(), is( "default.view.column.childCount" ) );
        assertThat( results[0].isOK(), is( true ));
    }

    @Test
    public void shouldTestViewColumnCountValidationFailure() throws Exception {
        Vdb vdb = createVdb("myVDB");
        Model model = addModel(vdb,"myModel");
        View view = addView(model,"myView");

        final Result[] results = _repo.getValidationManager().evaluate(getTransaction(), view, "default.view.column.childCount");
        assertThat( results.length, is( 1 ) );
        assertThat( results[0].getRuleId(), is( "default.view.column.childCount" ) );
        assertThat( results[0].isOK(), is( false ));
        assertThat( results[0].getLevel(), is( Level.ERROR));
    }

    @Test
    public void shouldTestViewColumnUniqueNameValidationSuccess() throws Exception {
        Vdb vdb = createVdb("myVDB");
        Model model = addModel(vdb,"myModel");
        View view = addView(model,"myView");
        addColumn(view, "myColumn1");
        addColumn(view, "myColumn2");

        final Result[] results = _repo.getValidationManager().evaluate(getTransaction(), view, "default.view.column.childSNSValidation");
        assertThat( results.length, is( 1 ) );
        assertThat( results[0].getRuleId(), is( "default.view.column.childSNSValidation" ) );
        assertThat( results[0].isOK(), is( true ));
    }

    @Test
    public void shouldTestViewColumnUniqueNameValidationFailure() throws Exception {
        Vdb vdb = createVdb("myVDB");
        Model model = addModel(vdb,"myModel");
        View view = addView(model,"myView");
        addColumn(view, "myColumn1");
        addColumn(view, "myColumn1");

        final Result[] results = _repo.getValidationManager().evaluate(getTransaction(), view, "default.view.column.childSNSValidation");
        assertThat( results.length, is( 1 ) );
        assertThat( results[0].getRuleId(), is( "default.view.column.childSNSValidation" ) );
        assertThat( results[0].isOK(), is( false ));
        assertThat( results[0].getLevel(), is( Level.ERROR));
    }

    // ==============================================================================================
    // ModelSource Validation Rules
    // ==============================================================================================

    @Test
    public void shouldTestModelSourceNodeNameValidationSuccess() throws Exception {
        Vdb vdb = createVdb("myVDB");
        Model model = addModel(vdb,"myModel");
        ModelSource modelSource = addSource(model,"mySource");

        final Result[] results = _repo.getValidationManager().evaluate(getTransaction(), modelSource, "default.modelSource.nodeName");
        assertThat( results.length, is( 1 ) );
        assertThat( results[0].getRuleId(), is( "default.modelSource.nodeName" ) );
        assertThat( results[0].isOK(), is( true ));
    }

    @Test
    public void shouldTestModelSourceNodeNameValidationFailure() throws Exception {
        Vdb vdb = createVdb("myVDB");
        Model model = addModel(vdb,"myModel");
        ModelSource modelSource = addSource(model,"1mySource");

        final Result[] results = _repo.getValidationManager().evaluate(getTransaction(), modelSource, "default.modelSource.nodeName");
        assertThat( results.length, is( 1 ) );
        assertThat( results[0].getRuleId(), is( "default.modelSource.nodeName" ) );
        assertThat( results[0].isOK(), is( false ));
        assertThat( results[0].getLevel(), is( Level.ERROR));
    }

    // ==============================================================================================
    // PushdownFunction Validation Rules
    // ==============================================================================================

    

    // ==============================================================================================
    // UserDefinedFunction Validation Rules
    // ==============================================================================================

    

    // ==============================================================================================
    // VirtualProcedure Validation Rules
    // ==============================================================================================

    

    // ==============================================================================================
    // StoredProcedure Validation Rules
    // ==============================================================================================

    

    // ==============================================================================================
    // AccessPattern Validation Rules
    // ==============================================================================================

    

    // ==============================================================================================
    // Index Validation Rules
    // ==============================================================================================

    

    // ==============================================================================================
    // PrimaryKey Validation Rules
    // ==============================================================================================

    @Test
    public void shouldTestPrimaryKeyNodeNameValidationSuccess() throws Exception {
        Vdb vdb = createVdb("myVDB");
        Model model = addModel(vdb,"myModel");
        Table table = addTable(model,"myTable");
        PrimaryKey pk = addPrimaryKey(table,"myPK");

        final Result[] results = _repo.getValidationManager().evaluate(getTransaction(), pk, "default.primaryKey.nodeName");
        assertThat( results.length, is( 1 ) );
        assertThat( results[0].getRuleId(), is( "default.primaryKey.nodeName" ) );
        assertThat( results[0].isOK(), is( true ));
    }

    @Test
    public void shouldTestPrimaryKeyNodeNameValidationFailure() throws Exception {
        Vdb vdb = createVdb("myVDB");
        Model model = addModel(vdb,"myModel");
        Table table = addTable(model,"myTable");
        PrimaryKey pk = addPrimaryKey(table,"1myPK");

        final Result[] results = _repo.getValidationManager().evaluate(getTransaction(), pk, "default.primaryKey.nodeName");
        assertThat( results.length, is( 1 ) );
        assertThat( results[0].getRuleId(), is( "default.primaryKey.nodeName" ) );
        assertThat( results[0].isOK(), is( false ));
        assertThat( results[0].getLevel(), is( Level.ERROR));
    }

    // ==============================================================================================
    // ForeignKey Validation Rules
    // ==============================================================================================

    @Test
    public void shouldTestForeignKeyNodeNameValidationSuccess() throws Exception {
        Vdb vdb = createVdb("myVDB");
        Model model = addModel(vdb,"myModel");
        Table table = addTable(model,"myTable");
        Table refTable = addTable(model,"refTable");
        ForeignKey fk = addForeignKey(table,"myFK",refTable);

        final Result[] results = _repo.getValidationManager().evaluate(getTransaction(), fk, "default.foreignKey.nodeName");
        assertThat( results.length, is( 1 ) );
        assertThat( results[0].getRuleId(), is( "default.foreignKey.nodeName" ) );
        assertThat( results[0].isOK(), is( true ));
    }

    @Test
    public void shouldTestForeignKeyNodeNameValidationFailure() throws Exception {
        Vdb vdb = createVdb("myVDB");
        Model model = addModel(vdb,"myModel");
        Table table = addTable(model,"myTable");
        Table refTable = addTable(model,"refTable");
        ForeignKey fk = addForeignKey(table,"1myFK",refTable);

        final Result[] results = _repo.getValidationManager().evaluate(getTransaction(), fk, "default.foreignKey.nodeName");
        assertThat( results.length, is( 1 ) );
        assertThat( results[0].getRuleId(), is( "default.foreignKey.nodeName" ) );
        assertThat( results[0].isOK(), is( false ));
        assertThat( results[0].getLevel(), is( Level.ERROR));
    }

    // ==============================================================================================
    // UniqueConstraint Validation Rules
    // ==============================================================================================

    @Test
    public void shouldTestUniqueConstraintNodeNameValidationSuccess() throws Exception {
        Vdb vdb = createVdb("myVDB");
        Model model = addModel(vdb,"myModel");
        Table table = addTable(model,"myTable");
        UniqueConstraint uc = addUniqueConstraint(table,"myUC");

        final Result[] results = _repo.getValidationManager().evaluate(getTransaction(), uc, "default.uniqueConstraint.nodeName");
        assertThat( results.length, is( 1 ) );
        assertThat( results[0].getRuleId(), is( "default.uniqueConstraint.nodeName" ) );
        assertThat( results[0].isOK(), is( true ));
    }

    @Test
    public void shouldTestUniqueConstraintNodeNameValidationFailure() throws Exception {
        Vdb vdb = createVdb("myVDB");
        Model model = addModel(vdb,"myModel");
        Table table = addTable(model,"myTable");
        UniqueConstraint uc = addUniqueConstraint(table,"1myUC");

        final Result[] results = _repo.getValidationManager().evaluate(getTransaction(), uc, "default.uniqueConstraint.nodeName");
        assertThat( results.length, is( 1 ) );
        assertThat( results[0].getRuleId(), is( "default.uniqueConstraint.nodeName" ) );
        assertThat( results[0].isOK(), is( false ));
        assertThat( results[0].getLevel(), is( Level.ERROR));
    }

    // ==============================================================================================
    // Column Validation Rules
    // ==============================================================================================

    @Test
    public void shouldTestColumnNodeNameValidationSuccess() throws Exception {
        Vdb vdb = createVdb("myVDB");
        Model model = addModel(vdb,"myModel");
        Table table = addTable(model,"myTable");
        Column col = addColumn(table,"myColumn");

        final Result[] results = _repo.getValidationManager().evaluate(getTransaction(), col, "default.column.nodeName");
        assertThat( results.length, is( 1 ) );
        assertThat( results[0].getRuleId(), is( "default.column.nodeName" ) );
        assertThat( results[0].isOK(), is( true ));
    }

    @Test
    public void shouldTestColumnNodeNameValidationFailure() throws Exception {
        Vdb vdb = createVdb("myVDB");
        Model model = addModel(vdb,"myModel");
        Table table = addTable(model,"myTable");
        Column col = addColumn(table,"1myColumn");

        final Result[] results = _repo.getValidationManager().evaluate(getTransaction(), col, "default.column.nodeName");
        assertThat( results.length, is( 1 ) );
        assertThat( results[0].getRuleId(), is( "default.column.nodeName" ) );
        assertThat( results[0].isOK(), is( false ));
        assertThat( results[0].getLevel(), is( Level.ERROR));
    }

    @Test
    public void shouldTestColumnDatatypeValidationSuccess() throws Exception {
        Vdb vdb = createVdb("myVDB");
        Model model = addModel(vdb,"myModel");
        Table table = addTable(model,"myTable");
        Column col = addColumn(table,"myColumn");
        col.setDatatypeName(getTransaction(), "string");

        final Result[] results = _repo.getValidationManager().evaluate(getTransaction(), col, "default.column.datatype.propertyValue");
        assertThat( results.length, is( 1 ) );
        assertThat( results[0].getRuleId(), is( "default.column.datatype.propertyValue" ) );
        assertThat( results[0].isOK(), is( true ));
    }

    @Test
    public void shouldTestColumnDatatypeValidationFailure() throws Exception {
        Vdb vdb = createVdb("myVDB");
        Model model = addModel(vdb,"myModel");
        Table table = addTable(model,"myTable");
        Column col = addColumn(table,"myColumn");

        final Result[] results = _repo.getValidationManager().evaluate(getTransaction(), col, "default.column.datatype.propertyValue");
        assertThat( results.length, is( 1 ) );
        assertThat( results[0].getRuleId(), is( "default.column.datatype.propertyValue" ) );
        assertThat( results[0].isOK(), is( false ));
        assertThat( results[0].getLevel(), is( Level.ERROR));
    }

    @Test
    public void shouldTestStringColumnLengthValidationSuccess() throws Exception {
        Vdb vdb = createVdb("myVDB");
        Model model = addModel(vdb,"myModel");
        Table table = addTable(model,"myTable");
        Column col = addColumn(table,"myColumn");
        col.setDatatypeName(getTransaction(), "string");
        col.setLength(getTransaction(), 1);

        final Result[] results = _repo.getValidationManager().evaluate(getTransaction(), col, "default.column.stringDatatype.lengthPropertyValue");
        assertThat( results.length, is( 1 ) );
        assertThat( results[0].getRuleId(), is( "default.column.stringDatatype.lengthPropertyValue" ) );
        assertThat( results[0].isOK(), is( true ));
    }

    @Test
    public void shouldStringColumnNoLengthValidationFailure() throws Exception {
        Vdb vdb = createVdb("myVDB");
        Model model = addModel(vdb,"myModel");
        Table table = addTable(model,"myTable");
        Column col = addColumn(table,"myColumn");
        col.setDatatypeName(getTransaction(), "string");

        final Result[] results = _repo.getValidationManager().evaluate(getTransaction(), col, "default.column.stringDatatype.lengthPropertyValue");
        assertThat( results.length, is( 1 ) );
        assertThat( results[0].getRuleId(), is( "default.column.stringDatatype.lengthPropertyValue" ) );
        assertThat( results[0].isOK(), is( false ));
        assertThat( results[0].getLevel(), is( Level.ERROR));
    }

    @Test
    public void shouldStringColumnWrongLengthValidationFailure() throws Exception {
        Vdb vdb = createVdb("myVDB");
        Model model = addModel(vdb,"myModel");
        Table table = addTable(model,"myTable");
        Column col = addColumn(table,"myColumn");
        col.setDatatypeName(getTransaction(), "string");
        col.setLength(getTransaction(), 0);

        final Result[] results = _repo.getValidationManager().evaluate(getTransaction(), col, "default.column.stringDatatype.lengthPropertyValue");
        assertThat( results.length, is( 1 ) );
        assertThat( results[0].getRuleId(), is( "default.column.stringDatatype.lengthPropertyValue" ) );
        assertThat( results[0].isOK(), is( false ));
        assertThat( results[0].getLevel(), is( Level.ERROR));
    }

    @Test
    public void shouldTestNumericColumnPrecisionValidationSuccess() throws Exception {
        Vdb vdb = createVdb("myVDB");
        Model model = addModel(vdb,"myModel");
        Table table = addTable(model,"myTable");
        Column col = addColumn(table,"myColumn");
        col.setDatatypeName(getTransaction(), "decimal");
        col.setPrecision(getTransaction(), 1);

        final Result[] results = _repo.getValidationManager().evaluate(getTransaction(), col, "default.column.numericDatatype.precisionPropertyValue");
        assertThat( results.length, is( 1 ) );
        assertThat( results[0].getRuleId(), is( "default.column.numericDatatype.precisionPropertyValue" ) );
        assertThat( results[0].isOK(), is( true ));
    }

    @Test
    public void shouldTestNumericColumnNoPrecisionValidationFailure() throws Exception {
        Vdb vdb = createVdb("myVDB");
        Model model = addModel(vdb,"myModel");
        Table table = addTable(model,"myTable");
        Column col = addColumn(table,"myColumn");
        col.setDatatypeName(getTransaction(), "decimal");

        final Result[] results = _repo.getValidationManager().evaluate(getTransaction(), col, "default.column.numericDatatype.precisionPropertyValue");
        assertThat( results.length, is( 1 ) );
        assertThat( results[0].getRuleId(), is( "default.column.numericDatatype.precisionPropertyValue" ) );
        assertThat( results[0].isOK(), is( false ));
        assertThat( results[0].getLevel(), is( Level.ERROR));
    }

    @Test
    public void shouldTestNumericColumnWrongPrecisionValidationFailure() throws Exception {
        Vdb vdb = createVdb("myVDB");
        Model model = addModel(vdb,"myModel");
        Table table = addTable(model,"myTable");
        Column col = addColumn(table,"myColumn");
        col.setDatatypeName(getTransaction(), "decimal");
        col.setPrecision(getTransaction(), 0);

        final Result[] results = _repo.getValidationManager().evaluate(getTransaction(), col, "default.column.numericDatatype.precisionPropertyValue");
        assertThat( results.length, is( 1 ) );
        assertThat( results[0].getRuleId(), is( "default.column.numericDatatype.precisionPropertyValue" ) );
        assertThat( results[0].isOK(), is( false ));
        assertThat( results[0].getLevel(), is( Level.ERROR));
    }

    // ==============================================================================================
    // Parameter Validation Rules
    // ==============================================================================================


    // ==============================================================================================
    // DataTypeResultSet Validation Rules
    // ==============================================================================================


    // ==============================================================================================
    // TabularResultSet Validation Rules
    // ==============================================================================================


    // ==============================================================================================
    // ResultSetColumn Validation Rules
    // ==============================================================================================

}
