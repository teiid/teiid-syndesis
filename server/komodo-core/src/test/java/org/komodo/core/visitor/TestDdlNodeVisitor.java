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
package org.komodo.core.visitor;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.fail;

import java.util.ArrayList;
import java.util.List;

import org.junit.Ignore;
import org.junit.Test;
import org.komodo.core.AbstractLocalRepositoryTest;
import org.komodo.metadata.DefaultMetadataInstance;
import org.komodo.metadata.TeiidConnectionProvider;
import org.komodo.spi.lexicon.sql.teiid.TeiidSqlConstants;
import org.komodo.spi.repository.KomodoObject;
import org.mockito.Mockito;
import org.teiid.modeshape.sequencer.vdb.lexicon.VdbLexicon;

/**
 *
 */
@SuppressWarnings({"nls", "javadoc"})
public class TestDdlNodeVisitor extends AbstractLocalRepositoryTest {

    /**
     *
     */
    private static final String SEQUENCE_DDL_PATH = "\\/ddl[0-9]+\\.ddl\\/";

    /*
     * Since the options arguments can occur in any order when the visitor
     * visits them, we need to break the line down and compare the arguments
     * themselves rather than rely on a simple options1.equals(options2)
     */
    protected void compareOptions(String visLine, String ddlLine) {
        String ddlOptions = ddlLine.substring(ddlLine.indexOf(TeiidSqlConstants.Reserved.OPTIONS));
        String visOptions = visLine.substring(visLine.indexOf(TeiidSqlConstants.Reserved.OPTIONS));

        String optionsPattern = "OPTIONS \\(([\"[a-zA-Z0-9_:]+\"\\.]*\"[a-zA-Z0-9_:]+\" [']?[a-zA-Z0-9\\. ]+[']?(, )?)+\\)[;|,]?";
        assertTrue("Test DDL Options do not match expected pattern: " + NEW_LINE + ddlOptions, ddlOptions.matches(optionsPattern));
        assertTrue("Visitor Options do not match expected pattern: " + NEW_LINE + visOptions, visOptions.matches(optionsPattern));

        // Remove the OPTIONS SPACE and BRACKETS
        String optionsPrefixPattern = "OPTIONS \\(";
        ddlOptions = ddlOptions.replaceAll(optionsPrefixPattern, EMPTY_STRING);
        visOptions = visOptions.replaceAll(optionsPrefixPattern, EMPTY_STRING);

        String optionsPostFixPattern = "\\)[;|,]?";
        ddlOptions = ddlOptions.replaceAll(optionsPostFixPattern, EMPTY_STRING);
        visOptions = visOptions.replaceAll(optionsPostFixPattern, EMPTY_STRING);

        // Should leave just the "key 'value', key 'value', key TRUE|FALSE
        String[] ddlTokens = ddlOptions.split(COMMA);
        String[] visTokens = visOptions.split(COMMA);

        List<String> ddlTokenList = new ArrayList<String>();
        List<String> visTokenList = new ArrayList<String>();

        for (String ddlToken : ddlTokens) {
            ddlTokenList.add(ddlToken.trim());
        }

        for (String visToken : visTokens) {
            visTokenList.add(visToken.trim());
        }

        assertEquals("Visitor tokens do not equal the options expected from the test list:" + NEW_LINE +
                     "DDL List -->" + ddlTokenList + NEW_LINE +
                     "Visitor List --> " + visTokenList, ddlTokenList.size(), visTokenList.size());

        assertTrue("Visitor tokens do not contain all the options expected from the test list:" + NEW_LINE +
                   "DDL List -->" + ddlTokenList + NEW_LINE +
                   "Visitor List --> " + visTokenList, visTokenList.containsAll(ddlTokenList));
    }

    protected void compare(String ddl, DdlNodeVisitor visitor) {
        assertNotNull(visitor);

        String visitorDDL = visitor.getDdl();
        assertNotNull(visitorDDL);

        String[] visLines = visitorDDL.split(NEW_LINE);
        String[] ddlLines = ddl.split(NEW_LINE);

        for (int i = 0; i < visLines.length; ++i) {
            String visLine = visLines[i];
            String ddlLine = ddlLines[i];

            if (visLine.equals(ddlLine))
                continue;

            if (! visLine.contains(TeiidSqlConstants.Reserved.OPTIONS))
                fail("Visitor output did not match ddl at line " + (i + 1) + ": DDL -->" + NEW_LINE + ddl + NEW_LINE + "VISITOR --> " + NEW_LINE + visitorDDL);

            //
            // Options are the wrong way around
            //
            compareOptions(visLine, ddlLine);
        }
    }

    protected void helpTest(String ddl, String expected, String... pathsToBeSequenced) throws Exception {
        KomodoObject workspace = _repo.komodoWorkspace(getTransaction());
        KomodoObject ddlObject = workspace.addChild(getTransaction(), "ddl-sequencing" + System.nanoTime(), VdbLexicon.Vdb.DECLARATIVE_MODEL);
        ddlObject.setProperty(getTransaction(), VdbLexicon.Model.MODEL_DEFINITION, ddl);

//        traverse(getTransaction(), ddlObject);

        //
        // Should wait for the sequencers to complete
        //
        commit();

        TeiidConnectionProvider provider = Mockito.mock(TeiidConnectionProvider.class);
        DefaultMetadataInstance instance = new DefaultMetadataInstance(provider);
        DdlNodeVisitor visitor = new DdlNodeVisitor(instance.getVersion(), instance.getDataTypeService(), false);
        visitor.visit(getTransaction(), ddlObject);

        compare(expected, visitor);
    }

    @Test(timeout = 5000000)
    public void testForeignTable() throws Exception {
        String ddl = "CREATE FOREIGN TABLE G1 (" + NEW_LINE +
                TAB + "e1 integer," + NEW_LINE +
                TAB + "e2 string(10)," + NEW_LINE +
                TAB + "e3 date NOT NULL," + NEW_LINE +
                TAB + "e4 bigdecimal(12,3)," + NEW_LINE +
                TAB + "e5 integer AUTO_INCREMENT OPTIONS (\"UUID\" 'uuid', \"NAMEINSOURCE\" 'nis', \"SELECTABLE\" 'FALSE')," + NEW_LINE +
                TAB + "e6 string DEFAULT 'hello'," + NEW_LINE +
                TAB + "PRIMARY KEY(e1)," + NEW_LINE +
                TAB + "UNIQUE(e2)," + NEW_LINE +
                TAB + "UNIQUE(e3)," + NEW_LINE +
                TAB + "INDEX(e5)," + NEW_LINE +
                TAB + "INDEX(e6)" + NEW_LINE +
                ") OPTIONS (\"ANNOTATION\" 'Test Table', \"CARDINALITY\" '12', \"FOO\" 'BAR', \"UPDATABLE\" 'true', \"UUID\" 'uuid2');";

        helpTest(ddl, ddl, SEQUENCE_DDL_PATH + "G1");
    }

    @Test(timeout = 5000000)
    public void testForeignTable2() throws Exception {
        String ddl = "CREATE FOREIGN TABLE G1 (" + NEW_LINE +
                TAB + "street string," + NEW_LINE +
                TAB + "zipcode string," + NEW_LINE +
                TAB + "building string," + NEW_LINE +
                TAB + "coord object[]," + NEW_LINE +
                TAB + "test string" + NEW_LINE +
                ") OPTIONS (\"UPDATABLE\" 'true', \"UUID\" 'uuid2');";

        helpTest(ddl, ddl, SEQUENCE_DDL_PATH + "G1");
    }

    @Test(timeout = 5000000)
    public void testMultiKeyPK() throws Exception {
        String ddl = "CREATE FOREIGN TABLE G1 (" + NEW_LINE +
                TAB + "e1 integer," + NEW_LINE +
                TAB + "e2 varchar," + NEW_LINE +
                TAB + "e3 date," + NEW_LINE +
                TAB + "PRIMARY KEY(e1, e2)" + NEW_LINE +
                ");";

        helpTest(ddl, ddl, SEQUENCE_DDL_PATH + "G1");
    }

    @Test(timeout = 5000000)
    public void testConstraints2() throws Exception {
        String ddl = "CREATE FOREIGN TABLE G1 (" + NEW_LINE +
                TAB + "e1 integer," + NEW_LINE +
                TAB + "e2 varchar," + NEW_LINE +
                TAB + "e3 date," + NEW_LINE +
                TAB + "ACCESSPATTERN(e1)," + NEW_LINE +
                TAB + "ACCESSPATTERN(e2, e3)," + NEW_LINE +
                TAB + "UNIQUE(e1) OPTIONS (\"x\" 'true')" + NEW_LINE +
                ");";
        helpTest(ddl, ddl, SEQUENCE_DDL_PATH + "G1");
    }

    @Test(timeout = 5000000)
    public void testFK() throws Exception {
        String ddl = "CREATE FOREIGN TABLE G1 (" + NEW_LINE +
                TAB + "\"g1-e1\" integer," + NEW_LINE +
                TAB + "g1e2 varchar," + NEW_LINE +
                TAB + "PRIMARY KEY(\"g1-e1\", g1e2)" + NEW_LINE +
                ");" +
                NEW_LINE + NEW_LINE +
                "CREATE FOREIGN TABLE G2 (" + NEW_LINE +
                TAB + "g2e1 integer," + NEW_LINE +
                TAB + "g2e2 varchar," + NEW_LINE +
                TAB + "FOREIGN KEY(g2e1, g2e2) REFERENCES G1 (\"g1-e1\", g1e2)" + NEW_LINE +
                ");";

        helpTest(ddl, ddl, SEQUENCE_DDL_PATH + "G1");
    }

    @Test(timeout = 5000000)
    public void testOptionalFK() throws Exception {
        String ddl = "CREATE FOREIGN TABLE G1 (" + NEW_LINE +
                TAB + "g1e1 integer," + NEW_LINE +
                TAB + "g1e2 varchar," + NEW_LINE +
                TAB + "PRIMARY KEY(g1e1, g1e2)" + NEW_LINE +
                ");" + NEW_LINE +
                NEW_LINE +
                "CREATE FOREIGN TABLE G2 (" + NEW_LINE +
                TAB + "g2e1 integer," + NEW_LINE +
                TAB + "g2e2 varchar," + NEW_LINE +
                TAB + "PRIMARY KEY(g2e1, g2e2)," + NEW_LINE +
                TAB + "FOREIGN KEY(g2e1, g2e2) REFERENCES G1" + NEW_LINE +
                ");";

        helpTest(ddl, ddl, SEQUENCE_DDL_PATH + "G1");
    }

    @Test(timeout = 5000000)
    public void testFKWithOptions() throws Exception {
        String ddl = "CREATE FOREIGN TABLE G1 (" + NEW_LINE +
                TAB + "g1e1 integer," + NEW_LINE +
                TAB + "g1e2 string," + NEW_LINE +
                TAB + "PRIMARY KEY(g1e1, g1e2)" + NEW_LINE +
                ");" + NEW_LINE +
                NEW_LINE +
                "CREATE FOREIGN TABLE G2 (" + NEW_LINE +
                TAB + "g2e1 integer," + NEW_LINE +
                TAB + "g2e2 string," + NEW_LINE +
                TAB + "PRIMARY KEY(g2e1, g2e2)," + NEW_LINE +
                TAB + "FOREIGN KEY(g2e1, g2e2) REFERENCES G1 OPTIONS (\"NAMEINSOURCE\" 'g1Relationship')" + NEW_LINE +
                ");";
        helpTest(ddl, ddl, SEQUENCE_DDL_PATH + "G1", SEQUENCE_DDL_PATH + "G2");
    }

    @Test(timeout = 5000000)
    public void testMultipleCommands() throws Exception {
        String ddl = "CREATE VIEW V1" + NEW_LINE +
                "AS" + NEW_LINE +
                "SELECT * FROM PM1.G1;" + NEW_LINE +
                NEW_LINE +
                "CREATE VIRTUAL PROCEDURE FOO(IN P1 integer) RETURNS TABLE (e1 integer, e2 string)" + NEW_LINE +
                "AS" + NEW_LINE +
                "SELECT * FROM PM1.G1;;";

        helpTest(ddl, ddl, SEQUENCE_DDL_PATH + "V1\\/tsql:query", SEQUENCE_DDL_PATH + "FOO\\/tsql:query");
    }

    @Test(timeout = 5000000)
    public void testView() throws Exception {
        String ddl = "CREATE VIEW G1 (" + NEW_LINE +
                TAB + "e1 integer," + NEW_LINE +
                TAB + "e2 varchar" + NEW_LINE +
                ") OPTIONS (\"CARDINALITY\" '1234567954432')" + NEW_LINE +
                "AS" + NEW_LINE +
                "SELECT e1, e2 FROM foo.bar;";
        helpTest(ddl, ddl, SEQUENCE_DDL_PATH + "G1\\/tsql:query");
    }

    @Test(timeout = 5000000)
    public void testInsteadOfTrigger() throws Exception {
        String ddl = "CREATE VIEW G1 (" + NEW_LINE +
                TAB + "e1 integer," + NEW_LINE +
                TAB + "e2 varchar" + NEW_LINE +
                ")" + NEW_LINE +
                "AS" + NEW_LINE +
                "SELECT * FROM foo;" + NEW_LINE +
                "" + NEW_LINE +
                "CREATE TRIGGER ON G1 INSTEAD OF INSERT AS" + NEW_LINE +
                "FOR EACH ROW" + NEW_LINE +
                "BEGIN ATOMIC" + NEW_LINE +
                "INSERT INTO g1 (e1, e2) VALUES (1, 'trig');" + NEW_LINE +
                "END;";
        helpTest(ddl, ddl, SEQUENCE_DDL_PATH + "G1\\/tsql:query");
    }

    @Test(timeout = 5000000)
    public void testSourceProcedure() throws Exception {
        String ddl = "CREATE FOREIGN PROCEDURE myProc(OUT p1 boolean, IN p2 varchar, INOUT p3 bigdecimal) RETURNS TABLE (r1 string, r2 bigdecimal)" + NEW_LINE +
                "OPTIONS (\"UUID\" 'uuid', \"ANNOTATION\" 'desc', \"NAMEINSOURCE\" 'nis', \"UPDATECOUNT\" '2', \"RANDOM\" 'any')";
        helpTest(ddl, ddl, SEQUENCE_DDL_PATH + "myProc");
    }

    @Test(timeout = 5000000)
    public void testSourceProcedureNoReturn() throws Exception {
        String ddl = "CREATE FOREIGN PROCEDURE saveFile(IN filePath string, IN file object OPTIONS (\"ANNOTATION\" 'The contents to save.  Can be one of CLOB, BLOB, or XML'))" + NEW_LINE +
                "OPTIONS (\"ANNOTATION\" 'Saves the given value to the given path.  Any existing file will be overriden.')";
        helpTest(ddl, ddl, SEQUENCE_DDL_PATH + "saveFile");
    }

    @Test(timeout = 5000000)
    public void testSourceProcedureIntegerReturn() throws Exception {
        String ddl = "CREATE FOREIGN PROCEDURE SourceProc(IN filePath string, IN file object) RETURNS integer" + NEW_LINE +
                "OPTIONS (\"ANNOTATION\" 'hello world')";
        helpTest(ddl, ddl, SEQUENCE_DDL_PATH + "SourceProc");
    }

    @Test( timeout = 5000000 )
    public void testPushdownFunctionNoArgs() throws Exception {
        String ddl = "CREATE FOREIGN FUNCTION SourceFunc() RETURNS integer" + NEW_LINE +
                "OPTIONS (\"UUID\" 'hello world');";
        helpTest(ddl, ddl, SEQUENCE_DDL_PATH + "SourceFunc");
    }

    @Test( timeout = 5000000 )
    public void testNonPushdownFunction() throws Exception {
        String ddl = "CREATE VIRTUAL FUNCTION SourceFunc(p1 integer, p2 varchar) RETURNS integer" + NEW_LINE +
                "OPTIONS (\"JAVA_CLASS\" 'foo', \"JAVA_METHOD\" 'bar');";
        helpTest(ddl, ddl, SEQUENCE_DDL_PATH + "SourceFunc");
    }

    @Test( timeout = 5000000 )
    public void testSourceProcedureVariadic() throws Exception {
        String ddl = "CREATE FOREIGN PROCEDURE myProc(OUT p1 boolean, VARIADIC p3 bigdecimal) RETURNS TABLE (r1 string, r2 bigdecimal)";
        helpTest(ddl, ddl, SEQUENCE_DDL_PATH + "myProc");
    }

    @Test( timeout = 5000000 )
    @Ignore // Currently functions like upper not supported in DdlSequencer - see MODE-2398
    public void testViewFBI() throws Exception {
        String ddl = "CREATE VIEW G1 (" + NEW_LINE +
                            TAB + "\"a e1\" integer," + NEW_LINE +
                            TAB + "\"a e2\" varchar," + NEW_LINE +
                            TAB + "INDEX(\"a e1\", upper(\"a e2\"))" + NEW_LINE +
                            ")" + NEW_LINE +
                            "AS" + NEW_LINE +
                            "SELECT e1, e2 FROM foo.bar;";
        helpTest(ddl, ddl, SEQUENCE_DDL_PATH + "G1\\/tsql:query");
    }

    @Test( timeout = 5000000 )
    public void testNamespaces() throws Exception {
        String ddl = "SET NAMESPACE 'http://www.teiid.org/ext/relational/2012' AS teiid_rel;" + NEW_LINE +
                            "SET NAMESPACE 'some long thing' AS n1;" + NEW_LINE +
                            NEW_LINE +
                            "CREATE VIEW G1 (" + NEW_LINE +
                            TAB + "a integer," + NEW_LINE +
                            TAB + "b varchar" + NEW_LINE +
                            ") OPTIONS (\"teiid_rel:x\" 'false', \"n1:z\" 'stringval')" + NEW_LINE +
                            "AS" + NEW_LINE +
                            "SELECT e1, e2 FROM foo.bar;";
        helpTest(ddl, ddl, SEQUENCE_DDL_PATH + "G1\\/tsql:query");
    }

    @Ignore
    @Test( timeout = 5000000 )
    public void testGlobalTemporaryTable() throws Exception {
        String ddl = "CREATE GLOBAL TEMPORARY TABLE myTemp (" + NEW_LINE +
                            TAB + "x string," + NEW_LINE +
                            TAB + "y SERIAL," + NEW_LINE +
                            TAB + "PRIMARY KEY(x)" + NEW_LINE +
                            ")";
        helpTest(ddl, ddl, SEQUENCE_DDL_PATH + "myTemp");
    }

    @Test( timeout = 5000000 )
    public void testArrayTypes() throws Exception {
        String ddl = "CREATE FOREIGN PROCEDURE myProc(OUT p1 boolean, IN p2 string, INOUT p3 bigdecimal) RETURNS TABLE (r1 string(100)[], r2 bigdecimal[][])";
        helpTest(ddl, ddl, SEQUENCE_DDL_PATH + "myProc");
    }

    @Test(timeout = 5000000)
    public void testOptions() throws Exception {
        String ddl = "CREATE FOREIGN TABLE G1 (" + NEW_LINE +
                TAB + "e1 integer" + NEW_LINE +
                ") OPTIONS (\"foo\".\"bar\" 'Test Table', \"foo\" '12', \"FOO\" 'BAR', \"a\".\"b\".\"c\".\"d\" 'true');";
        helpTest(ddl, ddl, SEQUENCE_DDL_PATH + "G1");
    }

}
