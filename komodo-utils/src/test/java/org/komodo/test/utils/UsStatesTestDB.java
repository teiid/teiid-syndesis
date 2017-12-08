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
package org.komodo.test.utils;

import java.io.InputStream;
import java.sql.Connection;
import java.sql.DriverManager;
import java.sql.PreparedStatement;
import java.sql.SQLException;
import org.h2.tools.Server;
import org.komodo.spi.constants.StringConstants;
import org.komodo.spi.repository.PersistenceType;

public class UsStatesTestDB implements StringConstants {

    private static final String DB_DRIVER = PersistenceType.H2.getDriver();
    private static final String DB_CONNECTION = "jdbc:h2:mem:usstates;DB_CLOSE_DELAY=-1";
    private static final String DB_USER = EMPTY_STRING;
    private static final String DB_PASSWORD = EMPTY_STRING;

    private PreparedStatement statesStmt;
    private Server tcpServer;

    public UsStatesTestDB() throws Exception {
        initDb();
    }

    private Connection getConnection() throws Exception {
        try {
            Class.forName(DB_DRIVER);
        } catch (ClassNotFoundException e) {
            throw new Exception("Cannot find driver for test US States DB", e);
        }

        Connection connection;
        try {
            connection = DriverManager.getConnection(DB_CONNECTION, DB_USER, DB_PASSWORD);
            connection.setAutoCommit(true);
        } catch (SQLException e) {
            throw new Exception("Cannot get connection to test US States DB", e);
        }
        return connection;
    }

    private void initDb() throws Exception {
        Connection connection = getConnection();

        tcpServer = Server.createTcpServer().start();
        
        try {
            if (statesStmt == null) {
                InputStream statesSqlStream = TestUtilities.usStatesSql();
                String statesSql = TestUtilities.streamToString(statesSqlStream);
                statesStmt = connection.prepareStatement(statesSql);
            }

            statesStmt.execute();
        } catch (SQLException ex) {
            throw new Exception("Failed to initialise US States DB with data", ex);
        } finally {
            if (connection != null)
                connection.close();
        }
    }

    public void dispose() throws Exception {
        try {
            if (tcpServer != null)
                tcpServer.stop();

        } catch (Exception ex) {
            throw new Exception("Failed to dispose the US States DB", ex);
        }
    }
}
