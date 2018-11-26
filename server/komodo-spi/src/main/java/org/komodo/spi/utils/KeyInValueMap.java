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
package org.komodo.spi.utils;

import java.util.AbstractMap;
import java.util.HashMap;
import java.util.Map;
import java.util.Set;

/**
 * A {@link Map} facade which stores values that also reference their
 * own keys.
 *
 * @param <K> key
 * @param <V> value
 */
@SuppressWarnings( "unchecked" )
public class KeyInValueMap<K, V> extends AbstractMap<K, V> {
	
	private final Map<K, V> instance;

    /**
     * Adapter interface that clients of the class should implement
     * and pass to its constructor so that the key (K) can be derived
     * from the value (V).
     *
     * @param <K> key
     * @param <V> value
     */
    public interface KeyFromValueAdapter<K, V> {

        /**
         * Get the key from the value
         *
         * @param value
         *
         * @return key (K) from the value (V)
         */
        K getKey(V value);

    }

    private KeyFromValueAdapter<K, V> adapter;

    /**
     * Create a new instance
     *
     * @param adapter that can convert from the value into the key
     */
    public KeyInValueMap(KeyFromValueAdapter<K, V> adapter) {
        this(adapter, new HashMap<>());
    }
    
    /**
     * Create a new instance, given the existing map
     *
     * @param adapter that can convert from the value into the key
     * @param map
     */
    public KeyInValueMap(KeyFromValueAdapter<K, V> adapter, Map<K, V> map) {
        this.adapter = adapter;
        this.instance = map;
    }

    @Override
    public V put(K key, V value) {
        throw new UnsupportedOperationException("Use add rather than put since the key is part of the value"); //$NON-NLS-1$
    }

    /**
     * Add a value to this map where its key will be derived
     * by the {@link KeyFromValueAdapter}
     *
     * @param value the Value
     *
     * @return true if the value was added.
     */
    public boolean add(V value) {
    	return instance.putIfAbsent(adapter.getKey(value), value) == null;
    }

    /**
     * Remove a value from this map where the key will
     * be determined by the {@link KeyFromValueAdapter}
     *
     * @param value the Value
     *
     * @return removed value or null.
     */
    @Override
    public V remove(Object value) {
    	try {
    		return instance.remove(adapter.getKey((V)value));
    	} catch (ClassCastException e) {
    		return instance.remove(value);
    	}
    }

	@Override
	public Set<Entry<K, V>> entrySet() {
		return instance.entrySet();
	}

}
