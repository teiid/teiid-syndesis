/**
 * @license
 * Copyright 2017 JBoss Inc
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *      http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

import { Identifiable } from "../../shared/identifiable";
import { SortDirection } from "../../shared/sort-direction.enum";
import { SyndesisLoadingState } from "./syndesis-loading-state.enum";

export class SyndesisSourceStatus implements Identifiable< string > {

  private sourceName: string;
  private hasTeiidSource: boolean;
  private vdbName: string;
  private vdbState: string;
  private errors: string[] = [];
  private schemaState: string;
  private schemaVdbName: string;
  private schemaModelName: string;
  private vdbLoadingState: SyndesisLoadingState;
  private schemaLoadingState: SyndesisLoadingState;

  /**
   * Create a ServiceCatalogSource from its json representation
   * @param {Object} json the JSON representation of a ServiceCatalogSource
   * @returns {ServiceCatalogSource} the new ServiceCatalogSource (never null)
   */
  public static create( json: object = {} ): SyndesisSourceStatus {
    const conn = new SyndesisSourceStatus();
    conn.setValues( json );
    return conn;
  }

  /**
   * Sorts the provided syndesis source statuses in the specified sort direction
   * @param {SyndesisSourceStatus[]} syndesisSrcStatuses the statuses being sorted
   * @param {SortDirection} sortDirection the sort direction
   */
  public static sort( syndesisSrcStatuses: SyndesisSourceStatus[],
                      sortDirection: SortDirection ): void {
    syndesisSrcStatuses.sort( ( thisStatus: SyndesisSourceStatus, thatStatus: SyndesisSourceStatus ) => {
      const result = thisStatus.compareTo( thatStatus );

      if ( sortDirection === SortDirection.DESC ) {
        return result * -1;
      }

      return result;
    } );
  }

  constructor() {
    // nothing to do
  }

  /**
   * See {Identifiable}.
   */
  public compareTo( that: SyndesisSourceStatus ): number {
    let result = 0;

    if ( this.getId() ) {
      if ( that.getId() ) {
        // both have an ID
        result = this.getId().localeCompare( that.getId() );
      } else {
        // thatItem does not have an ID
        result = 1;
      }
    } else if ( that.getId() ) {
      // thisItem does not have an ID and thatItem does
      result = -1;
    }

    return result;
  }

  /**
   * Get the syndesis source status id
   * @returns {string} the syndesis source status identifier (can be null)
   */
  public getId(): string {
    return this.sourceName;
  }

  /**
   * Get the syndesis source status name
   * @returns {string} the syndesis source status name (can be null)
   */
  public getName(): string {
    return this.sourceName;
  }

  /**
   * Determines whether syndesis source has a corresponding teiid VDB
   * @returns {boolean} the teiid source status
   */
  public hasTeiidSrc(): boolean {
    return this.hasTeiidSource;
  }

  /**
   * Get the syndesis source vdb loading state
   * @returns {SyndesisLoadingState} the syndesis source vdb loading state (cannot be null)
   */
  public getVdbState(): SyndesisLoadingState {
    return this.vdbLoadingState;
  }

  /**
   * Get the syndesis source status vdb name
   * @returns {string} the syndesis source status vdb name (can be null)
   */
  public getVdbName(): string {
    return this.vdbName;
  }

  /**
   * Get the syndesis source status vdb errors
   * @returns {string[]} the syndesis source status vdb errors (can be empty but never null)
   */
  public getVdbErrors(): string[] {
    return this.errors;
  }

  /**
   * Get the syndesis source schema loading state
   * @returns {SyndesisLoadingState} the syndesis source schema loading state (cannot be null)
   */
  public getSchemaState(): SyndesisLoadingState {
    return this.schemaLoadingState;
  }

  /**
   * Get the syndesis source status schema vdb name
   * @returns {string} the syndesis source status schema vdb name (can be null)
   */
  public getSchemaVdbName(): string {
    return this.schemaVdbName;
  }

  /**
   * Get the syndesis source status schema model name
   * @returns {string} the syndesis source status schema model name (can be null)
   */
  public getSchemaModelName(): string {
    return this.schemaModelName;
  }

  /**
   * Get ready state for the source.  The source has a teiid source binding, the vdb is active, and the schema is active.
   */
  public get isReady( ): boolean {
    return this.hasTeiidSource && (this.vdbLoadingState === SyndesisLoadingState.ACTIVE) && (this.schemaLoadingState === SyndesisLoadingState.ACTIVE);
  }

  /**
   * Get a message for the overall status of the syndesis source
   * @returns {string} the overall syndesis status message
   */
  public getOverallStatusMessage(): string {
    if ( !this.hasTeiidSource ) {
      return "No teiid source binding!";
    }
    let message = "Teiid VDB state: " + this.vdbState;
    message += ";  Schema state: " + this.schemaState;
    return message;
  }

  /**
   * Set all object values using the supplied catalog source json
   * @param {Object} values
   */
  public setValues(values: object = {}): void {
    Object.assign(this, values);

    if (this.vdbState) {
      if (this.vdbState === "ACTIVE") {
        this.vdbLoadingState = SyndesisLoadingState.ACTIVE;
      } else if (this.vdbState === "FAILED") {
        this.vdbLoadingState = SyndesisLoadingState.FAILED;
      } else if (this.vdbState === "LOADING") {
        this.vdbLoadingState = SyndesisLoadingState.LOADING;
      } else if (this.vdbState === "MISSING") {
        this.vdbLoadingState = SyndesisLoadingState.MISSING;
      }
    }

    if (this.schemaState) {
      if (this.schemaState === "ACTIVE") {
        this.schemaLoadingState = SyndesisLoadingState.ACTIVE;
      } else if (this.schemaState === "FAILED") {
        this.schemaLoadingState = SyndesisLoadingState.FAILED;
      } else if (this.schemaState === "LOADING") {
        this.schemaLoadingState = SyndesisLoadingState.LOADING;
      } else if (this.schemaState === "MISSING") {
        this.schemaLoadingState = SyndesisLoadingState.MISSING;
      }
    }

  }

}
