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
package org.komodo.rest.relational.response;

import java.net.URI;
import java.net.URL;
import org.komodo.relational.profile.GitRepository;
import org.komodo.rest.AbstractKEntity;
import org.komodo.spi.KException;
import org.komodo.spi.repository.Repository.UnitOfWork;

public class RestGitRepository extends AbstractKEntity {

    /**
     * Label used to repository name
     */
    public static final String NAME_LABEL = "Name";

    /**
     * Label used to repository url
     */
    public static final String URL_LABEL = "URL";

    /**
     * Label used to repository branch
     */
    public static final String BRANCH_LABEL = "Branch";

    /**
     * Label used to repository user
     */
    public static final String USER_LABEL = "User";

    /**
     * Label used to repository password
     */
    public static final String PASSWORD_LABEL = "Password";

    /**
     * Label used to repository commit author
     */
    public static final String COMMIT_AUTHOR_LABEL = "CommitAuthor";

    /**
     * Label used to repository commit email
     */
    public static final String COMMIT_EMAIL_LABEL = "CommitEmail";

    /**
     * Constructor for use when deserializing
     */
    public RestGitRepository() {
        super();
    }

    /**
     * Constructor for use when serializing.
     * @param baseUri the base uri
     * @param gitRepository the git repository
     *
     * @throws KException if error occurs
     */
    public RestGitRepository(URI baseUri, GitRepository gitRepository, UnitOfWork transaction) throws KException {
        super(baseUri);

        setName(gitRepository.getName(transaction));
        setUrl(gitRepository.getUrl(transaction));
        setBranch(gitRepository.getBranch(transaction));
        setUser(gitRepository.getUser(transaction));
        setPassword(gitRepository.getPassword(transaction));
        setCommitAuthor(gitRepository.getCommitAuthor(transaction));
        setCommitEmail(gitRepository.getCommitEmail(transaction));
    }

    /**
     * @return the name
     */
    public String getName() {
        Object name = tuples.get(NAME_LABEL);
        return name != null ? name.toString() : null;
    }

    public void setName(String name) {
        tuples.put(NAME_LABEL, name);
    }

    /**
     * @return the url
     */
    public String getUrl() {
        Object url = tuples.get(URL_LABEL);
        return url != null ? url.toString() : null;
    }

    public void setUrl(URL url) {
        tuples.put(URL_LABEL, url);
    }

    /**
     * @return the branch
     */
    public String getBranch() {
        Object branch = tuples.get(BRANCH_LABEL);
        return branch != null ? branch.toString() : null;
    }

    public void setBranch(String branch) {
        tuples.put(BRANCH_LABEL, branch);
    }

    /**
     * @return the user
     */
    public String getUser() {
        Object user = tuples.get(USER_LABEL);
        return user != null ? user.toString() : null;
    }

    public void setUser(String user) {
        tuples.put(USER_LABEL, user);
    }

    /**
     * @return the password
     */
    public String getPassword() {
        Object pswd = tuples.get(PASSWORD_LABEL);
        return pswd != null ? pswd.toString() : null;
    }

    public void setPassword(String password) {
        tuples.put(PASSWORD_LABEL, password);
    }

    /**
     * @return the commit author
     */
    public String getCommitAuthor() {
        Object author = tuples.get(COMMIT_AUTHOR_LABEL);
        return author != null ? author.toString() : null;
    }

    public void setCommitAuthor(String commitAuthor) {
        tuples.put(COMMIT_AUTHOR_LABEL, commitAuthor);
    }

    /**
     * @return the commit email
     */
    public String getCommitEmail() {
        Object email = tuples.get(COMMIT_EMAIL_LABEL);
        return email != null ? email.toString() : null;
    }

    public void setCommitEmail(String commitEmail) {
        tuples.put(COMMIT_EMAIL_LABEL, commitEmail);
    }
}