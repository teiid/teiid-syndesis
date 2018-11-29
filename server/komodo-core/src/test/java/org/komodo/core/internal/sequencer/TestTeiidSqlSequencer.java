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
package org.komodo.core.internal.sequencer;

import javax.jcr.Node;
import org.junit.Test;
import org.komodo.spi.lexicon.sql.teiid.TeiidSqlLexicon.Block;
import org.komodo.spi.lexicon.sql.teiid.TeiidSqlLexicon.CommandStatement;
import org.komodo.spi.lexicon.sql.teiid.TeiidSqlLexicon.CreateProcedureCommand;
import org.komodo.spi.lexicon.sql.teiid.TeiidSqlLexicon.From;
import org.komodo.spi.lexicon.sql.teiid.TeiidSqlLexicon.GroupBy;
import org.komodo.spi.lexicon.sql.teiid.TeiidSqlLexicon.Query;
import org.komodo.spi.lexicon.sql.teiid.TeiidSqlLexicon.SPParameter;
import org.komodo.spi.lexicon.sql.teiid.TeiidSqlLexicon.Select;
import org.komodo.spi.lexicon.sql.teiid.TeiidSqlLexicon.StoredProcedure;
import org.komodo.spi.lexicon.sql.teiid.TeiidSqlLexicon.SubqueryContainer;

/**
 *
 */
@SuppressWarnings( {"javadoc", "nls"} )
public class TestTeiidSqlSequencer extends AbstractTestTeiidSqlSequencer {

    @Test
    public void testGroupByRollup() throws Exception {
        String sql = "SELECT a FROM m.g GROUP BY rollup(b, c)";
        Node fileNode = sequenceSql(sql, TSQL_QUERY);

        Node queryNode = verify(fileNode, Query.ID, Query.ID);

        Node selectNode = verify(queryNode, Query.SELECT_REF_NAME, Select.ID);
        verifyElementSymbol(selectNode, Select.SYMBOLS_REF_NAME, "a");

        Node fromNode = verify(queryNode, Query.FROM_REF_NAME, From.ID);
        verifyUnaryFromClauseGroup(fromNode, From.CLAUSES_REF_NAME, 1, "m.g");

        Node groupByNode = verify(queryNode, Query.GROUP_BY_REF_NAME, GroupBy.ID);
        verifyElementSymbol(groupByNode, GroupBy.SYMBOLS_REF_NAME,  1, "b");
        verifyElementSymbol(groupByNode, GroupBy.SYMBOLS_REF_NAME,  2, "c");
        verifyProperty(groupByNode, GroupBy.ROLLUP_PROP_NAME, true);
    }

    @Test
    public void testStoredQuery2SanityCheck() throws Exception {
        String sql = "BEGIN exec proc1('param1'); END";
        Node fileNode = sequenceSql(sql, TSQL_PROC_CMD);

        Node createProcNode = verify(fileNode, CreateProcedureCommand.ID, CreateProcedureCommand.ID);
        Node outerBlkNode = verify(createProcNode, CreateProcedureCommand.BLOCK_REF_NAME, Block.ID);
        Node cmdStmtNode = verify(outerBlkNode, Block.STATEMENTS_REF_NAME, CommandStatement.ID);

        Node storedProcNode = verify(cmdStmtNode, SubqueryContainer.COMMAND_REF_NAME, StoredProcedure.ID);
        verifyProperty(storedProcNode, StoredProcedure.PROCEDURE_NAME_PROP_NAME, "proc1");

        Node param1Node = verify(storedProcNode, StoredProcedure.PARAMETERS_REF_NAME, SPParameter.ID);
        verifyProperty(param1Node, SPParameter.PARAMETER_TYPE_PROP_NAME, 1);
        verifyProperty(param1Node, SPParameter.INDEX_PROP_NAME, 1);
        verifyConstant(param1Node, SPParameter.EXPRESSION_REF_NAME, "param1");
    }
}
