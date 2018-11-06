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

/**
 * VdbStatus model
 */
export class VdbStatus {

  private name: string;
  private deployedName: string;
  private version = "1";
  private active = false;
  private loading = false;
  private failed = false;
  private errors: string[] = [];

  /**
   * @param json the JSON representation of a VdbStatus
   * @returns the new VdbStatus (never null)
   */
  public static create( json: object = {} ): VdbStatus {
    const vdbStatus = new VdbStatus();
    vdbStatus.setValues( json );
    return vdbStatus;
  }

  constructor() {
    // nothing to do
  }

  /**
   * @returns the vdbStatus name
   */
  public getName(): string {
    return this.name;
  }

  /**
   * @returns the vdbStatus deployedName
   */
  public getDeployedName(): string {
    return this.deployedName;
  }

  /**
   * @returns the vdbStatus version (can be null)
   */
  public getVersion(): string {
    return this.version;
  }

  /**
   * @returns the vdbStatus active state
   */
  public isActive(): boolean {
    return this.active;
  }

  /**
   * @returns the vdbStatus loading state
   */
  public isLoading(): boolean {
    return this.loading;
  }

  /**
   * @returns the vdbStatus failed state
   */
  public isFailed(): boolean {
    return this.failed;
  }

  /**
   * @returns the errors (never null)
   */
  public getErrors(): string[] {
    return this.errors;
  }

  /**
   * @param name the vdbStatus name
   */
  public setName( name: string ): void {
    this.name = name;
  }

  /**
   * @param deployedName the vdbStatus deployedName
   */
  public setDeployedName( deployedName: string ): void {
    this.deployedName = deployedName;
  }

  /**
   * @param version the vdbStatus version (optional)
   */
  public setVersion( version?: string ): void {
    this.version = version ? version : "1";
  }

  /**
   * @param active the active state
   */
  public setActive( active: boolean ): void {
    this.active = active;
  }

  /**
   * @param loading the loading state
   */
  public setLoading( loading: boolean ): void {
    this.loading = loading;
  }

  /**
   * @param failed the failed state
   */
  public setFailed( failed: boolean ): void {
    this.failed = failed;
  }

  /**
   * @param errors the status errors
   */
  public setErrors( errors: string[] ): void {
    this.errors = errors;
  }

  /**
   * Set all object values using the supplied VdbStatus json
   * @param values
   */
  public setValues(values: object = {}): void {
    Object.assign(this, values);
  }

}
