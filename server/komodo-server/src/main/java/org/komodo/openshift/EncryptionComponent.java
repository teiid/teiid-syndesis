/*
 * Copyright (C) 2016 Red Hat, Inc.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
package org.komodo.openshift;

import java.io.File;
import java.io.IOException;
import java.util.HashMap;
import java.util.Map;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.komodo.KException;
import org.springframework.security.crypto.encrypt.Encryptors;
import org.springframework.security.crypto.encrypt.TextEncryptor;

import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.dataformat.yaml.YAMLFactory;

/**
 * Handy methods used to apply encryption to configured secrets.
 *
 */
public class EncryptionComponent {
    public static final String SYNDESIS_ENC_KEY = "SYNDESIS_ENC_KEY";
    public static final String ENCRYPTED_PREFIX = "\u00BBENC:";
    private static final Log logger = LogFactory.getLog(EncryptionComponent.class);

    private final TextEncryptor textEncryptor;
    private String encryptKey;

    public EncryptionComponent(String encryptKey) {
        if (encryptKey == null) {
            encryptKey = getEncryptKey();
        }
        if (encryptKey != null) {
            this.textEncryptor = Encryptors.text(encryptKey, "deadbeef");
        } else {
            this.textEncryptor = null;
        }
    }

    public EncryptionComponent(TextEncryptor encryptor) {
        this.textEncryptor = encryptor;
    }

    public static String stripPrefix(String value, String prefix) {
        return value != null && value.startsWith(prefix)?value.substring(prefix.length()):value;
    }

    public String encrypt(final String value) {
        String result = value;
        if( !value.startsWith(ENCRYPTED_PREFIX) && textEncryptor != null) {
            result = ENCRYPTED_PREFIX+textEncryptor.encrypt(value);
        }
        return result;
    }


    public String decrypt(final String value) throws KException {
        // value might not be encrypted...
        if( value == null ) {
            return null;
        }
        String result = value;
        if( result.startsWith(ENCRYPTED_PREFIX)) {
            TextEncryptor enc = textEncryptor;
            try {
                result = enc.decrypt(stripPrefix(result, ENCRYPTED_PREFIX));
            } catch (RuntimeException e) {
                // We could fail to decrypt the value..
                throw new KException(e);
            }
        }
        return result;
    }

    public Map<String, String> decrypt(Map<String, String> props) throws KException {
        if( props == null ) {
            return null;
        }
        HashMap<String, String> result = new HashMap<>();
        for (Map.Entry<String, String> entry : props.entrySet()) {
            result.put(entry.getKey(), decrypt(entry.getValue()));
        }
        return result;
    }

    private String getEncryptKey() {
        try {
            if (encryptKey == null) {
                this.encryptKey = System.getenv(SYNDESIS_ENC_KEY);
                if (this.encryptKey == null) {
                    final ObjectMapper mapper = new ObjectMapper(new YAMLFactory());
                    // this is syndesis-server's configuration file. When we need more than encryption key
                    // will need to pull out as configuration.
                    JsonNode config = mapper.readTree(new File("./config/application.yml"));
                    JsonNode encrypt = config.get("encrypt");
                    if (encrypt != null) {
                        this.encryptKey = encrypt.get("key").asText();
                    }
                }
            }
            return encryptKey;
        } catch (JsonProcessingException e) {
            logger.error("Failed to load decryptor key");
        } catch (IOException e) {
            logger.error("Failed to load decryptor key");
        }
        return null;
    }

    public void setEncryptKey(String encryptKey) {
        this.encryptKey = encryptKey;
    }
}
