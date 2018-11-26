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
