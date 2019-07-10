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
import static org.junit.Assert.assertThat;

import java.util.ArrayList;
import java.util.List;

import org.komodo.core.AbstractLocalRepositoryTest;
import org.komodo.relational.model.Column;
import org.komodo.relational.model.ForeignKey;
import org.komodo.relational.model.Model;
import org.komodo.relational.model.PrimaryKey;
import org.komodo.relational.model.Table;
import org.komodo.relational.model.UniqueConstraint;
import org.komodo.relational.model.View;
import org.komodo.relational.vdb.ModelSource;
import org.komodo.relational.vdb.Translator;
import org.komodo.relational.vdb.Vdb;
import org.komodo.relational.vdb.VdbImport;
import org.komodo.relational.workspace.WorkspaceManager;
import org.komodo.spi.repository.validation.Rule;
import org.teiid.modeshape.sequencer.vdb.lexicon.VdbLexicon;

@SuppressWarnings( { "javadoc", "nls" } )
public class RelationalValidationTest extends AbstractLocalRepositoryTest {

    protected static final String VDB_PATH = "/vdb/path/vdb.vdb";
    protected static final String ENTRY_PATH = "/vdb/entryPath";

    protected List<String> getRuleNames( final Rule[] rules ) throws Exception {
        List<String> ruleNames = new ArrayList<String>();
        for(Rule rule : rules) {
            ruleNames.add( rule.getName( getTransaction() ) );
        }
        return ruleNames;
    }

    protected Vdb createVdb( final String vdbName ) throws Exception {
        final WorkspaceManager mgr = WorkspaceManager.getInstance(_repo, getTransaction());
        final Vdb vdb = mgr.createVdb( getTransaction(), null, vdbName, VDB_PATH );

        assertThat( vdb.getPrimaryType( getTransaction() ).getName(), is( VdbLexicon.Vdb.VIRTUAL_DATABASE ) );
        assertThat( vdb.getName( getTransaction() ), is( vdbName ) );
        assertThat( vdb.getOriginalFilePath( getTransaction() ), is( VDB_PATH ) );
        return vdb;
    }

    protected Model addModel( final Vdb vdb,
                              final String modelName ) throws Exception {
        final Model model = vdb.addModel( getTransaction(), modelName );

        assertThat( model.getPrimaryType( getTransaction() ).getName(), is( VdbLexicon.Vdb.DECLARATIVE_MODEL ) );
        assertThat( model.getName( getTransaction() ), is( modelName ) );
        return model;
    }

    protected Translator addTranslator( final Vdb vdb,
                                        final String translatorName,
                                        final String translatorType ) throws Exception {
        final Translator translator = vdb.addTranslator( getTransaction(), translatorName, translatorType );

        assertThat( translator.getPrimaryType( getTransaction() ).getName(), is( VdbLexicon.Translator.TRANSLATOR ) );
        assertThat( translator.getName( getTransaction() ), is( translatorName ) );
        return translator;
    }

    protected VdbImport addVdbImport( final Vdb vdb,
                                      final String importName ) throws Exception {
        final VdbImport vdbImport = vdb.addImport( getTransaction(), importName );

        assertThat( vdbImport.getPrimaryType( getTransaction() ).getName(), is( VdbLexicon.ImportVdb.IMPORT_VDB ) );
        assertThat( vdbImport.getName( getTransaction() ), is( importName ) );
        return vdbImport;
    }

    protected Table addTable( final Model model,
                              final String tableName ) throws Exception {

        final Table table = model.addTable( getTransaction(), tableName );

        assertThat( table.getName( getTransaction() ), is( tableName ) );
        return table;
    }

    protected ModelSource addSource( final Model model,
                                     final String sourceName ) throws Exception {
        final ModelSource modelSource = model.addSource( getTransaction(), sourceName );

        assertThat( modelSource.getName( getTransaction() ), is( sourceName ) );
        return modelSource;
    }

    protected View addView( final Model model,
                            final String viewName ) throws Exception {
        final View view = model.addView( getTransaction(), viewName );

        assertThat( view.getName( getTransaction() ), is( viewName ) );
        return view;
    }

    protected Column addColumn( final Table table,
                                final String columnName ) throws Exception {
        final Column column = table.addColumn( getTransaction(), columnName );

        assertThat( column.getName( getTransaction() ), is( columnName ) );
        return column;
    }

    protected Column addColumn( final View view,
                                final String columnName ) throws Exception {
        final Column column = view.addColumn( getTransaction(), columnName );

        assertThat( column.getName( getTransaction() ), is( columnName ) );
        return column;
    }

    protected PrimaryKey addPrimaryKey( final Table table,
                                        final String pkName ) throws Exception {
        final PrimaryKey pk = table.setPrimaryKey( getTransaction(), pkName );

        assertThat( pk.getName( getTransaction() ), is( pkName ) );
        return pk;
    }

    protected ForeignKey addForeignKey( final Table table,
                                        final String fkName,
                                        final Table referencedTable ) throws Exception {
        final ForeignKey fk = table.addForeignKey( getTransaction(), fkName, referencedTable );

        assertThat( fk.getName( getTransaction() ), is( fkName ) );
        return fk;
    }

    protected UniqueConstraint addUniqueConstraint( final Table table,
                                                    final String ucName ) throws Exception {
        final UniqueConstraint uc = table.addUniqueConstraint( getTransaction(), ucName );

        assertThat( uc.getName( getTransaction() ), is( ucName ) );
        return uc;
    }

}
