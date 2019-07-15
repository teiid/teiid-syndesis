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

import static org.hamcrest.core.Is.is;
import static org.junit.Assert.assertThat;

import org.junit.Test;
import org.komodo.metadata.DataTypeService;
import org.komodo.metadata.TeiidConnectionProvider;
import org.komodo.metadata.internal.DefaultMetadataInstance;
import org.mockito.Mockito;

@SuppressWarnings( {"javadoc", "nls"} )
public class TestDdlNodeVisitorIdentifiers {
    private DdlNodeVisitor visitor = new DdlNodeVisitor(getDataTypeService(), true);

    protected DataTypeService getDataTypeService() {
    	TeiidConnectionProvider provider = Mockito.mock(TeiidConnectionProvider.class);
    	return new DefaultMetadataInstance(provider).getDataTypeService();
    }

    @Test
    public void testEscapeOptionKeyNonQuoted() {
        assertThat("Non-quoted key should be quoted", visitor.escapeOptionKey("abc"), is("\"abc\""));
    }

    @Test
    public void testEscapeOptionKeyQuoted() {
        assertThat("Opening and ending pair of quotes should not be duplicated",
                   visitor.escapeOptionKey("\"abc\""),
                   is("\"abc\""));
    }

    @Test
    public void testEscapeOptionKeyEscapeQuotes() {
        assertThat("Double quotes should be escaped", visitor.escapeOptionKey("ab\"c"), is("\"ab\"\"c\""));
    }

    @Test
    public void testEscapeOptionKeyNoSplit() {
        assertThat("Each component of key should be quoted", visitor.escapeOptionKey("a.b.c"), is("\"a\".\"b\".\"c\""));
    }

    @Test
    public void testEscapeOptionKeyDots() {
        assertThat("Non-quoted key should be quoted", visitor.escapeOptionKey("abc"), is("\"abc\""));
    }

    @Test
    public void testEscapeOptionKeyEscapeAllQuotes() {
        assertThat("Double quotes should be escaped", visitor.escapeOptionKey("abc.abc.a\"c"), is("\"abc\".\"abc\".\"a\"\"c\""));
    }

    @Test
    public void testEscapeOptionKeyEscapeAllQuotes2() {
        assertThat("Double quotes should be escaped", visitor.escapeOptionKey("ab\"\"c.abc"), is("\"ab\"\"\"\"c\".\"abc\""));
    }

    @Test
    public void testEscapeOptionKeyQuotedOneChar() {
        assertThat("Non-quoted key should be quoted", visitor.escapeOptionKey("a"), is("\"a\""));
    }

    @Test
    public void testEscapeOptionKeyNonQuotedOneChar() {
        assertThat("Non-quoted key should be quoted", visitor.escapeOptionKey("\"a\""), is("\"a\""));
    }

    @Test
    public void testEscapeSinglePartNoQuote() {
        assertThat("Identifiers should not be quoted by default", visitor.escapeSinglePart("abc"), is("abc"));
    }

    @Test
    public void testEscapeSinglePartHash() {
        assertThat("Identifier can start with @ or #", visitor.escapeSinglePart("#abc"), is("#abc"));
    }

    @Test
    public void testEscapeSinglePartAtSign() {
        assertThat("Identifier can start with @ or #", visitor.escapeSinglePart("@abc"), is("@abc"));
    }

    @Test
    public void testEscapeSinglePartTwoHashes() {
        assertThat("Contains special characters.It should be quoted", visitor.escapeSinglePart("##abc"), is("\"##abc\""));
    }

    @Test
    public void testEscapeSinglePartDigit() {
        assertThat("Identifiers should not be quoted by default", visitor.escapeSinglePart("abc1"), is("abc1"));
    }

    @Test
    public void testEscapeSinglePartUnderScore() {
        assertThat("Identifier can cotain '_'", visitor.escapeSinglePart("a_bc"), is("a_bc"));
    }

    @Test
    public void testEscapeSinglePartReserved() {
        assertThat("Reserved keyword. Must be quoted", visitor.escapeSinglePart("OPTIONS"), is("\"OPTIONS\""));
    }

    @Test
    public void testEscapeSinglePartSpecialChar() {
        assertThat("Special character. Must be quoted", visitor.escapeSinglePart("a&bc"), is("\"a&bc\""));
    }
}
