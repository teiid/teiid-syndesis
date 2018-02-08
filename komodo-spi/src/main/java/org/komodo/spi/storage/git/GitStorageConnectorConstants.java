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
package org.komodo.spi.storage.git;

import org.komodo.spi.storage.StorageConnectorConstants;

/**
 * Constants for use with the git storage connector
 */
public interface GitStorageConnectorConstants extends StorageConnectorConstants {

    /**
     * The path to the remote repository
     */
    public static final String REPO_PATH_PROPERTY = "repo-path-property";

    /**
     * The destination into which to clone the repository
     */
    public static final String REPO_DEST_PROPERTY = "repo-dest-property";

    /**
     * The branch to checkout
     */
    public static final String REPO_BRANCH_PROPERTY = "repo-branch-property";

    /**
     * The ssh private key
     */
    public static final String REPO_PRIVATE_KEY = "repo-private-key-property";

    /**
     * The ssh passphrase
     */
    public static final String REPO_PASSPHRASE = "repo-passphrase-property";

    /**
     * The known hosts key for this repository
     */
    public static final String REPO_KNOWN_HOSTS_ID = "repo-known-hosts-property";

    /**
     * The password property (used by http)
     */
    public static final String REPO_USERNAME = "repo-username-property";

    /**
     * The password property (used by both ssh and http)
     */
    public static final String REPO_PASSWORD = "repo-password-property";

    /**
     * The name of the author to be applied when writing a commit
     */
    public static final String AUTHOR_NAME_PROPERTY = "author-name-property";

    /**
     * The email of the author to be applied when writing a commit
     */
    public static final String AUTHOR_EMAIL_PROPERTY = "author-email-property";

}
