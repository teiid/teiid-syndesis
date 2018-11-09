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
import { ModuleWithProviders } from "@angular/core";
import { CommonModule } from "@angular/common";
import { NgModule } from "@angular/core";
import { FormsModule, ReactiveFormsModule } from "@angular/forms";
import { Http } from "@angular/http";
import { RouterModule } from "@angular/router";
import { AppSettingsService } from "../core/app-settings.service";
import { CoreModule } from "../core/core.module";
import { LoggerService } from "../core/logger.service";
import { DataserviceCardComponent } from "./dataservices-cards/dataservice-card/dataservice-card.component";
import { SelectionService } from "../core/selection.service";
import { DataservicesCardsComponent } from "./dataservices-cards/dataservices-cards.component";
import { DataservicesDetailsComponent } from "./dataservices-list/dataservices-details.component";
import { DataservicesListComponent } from "./dataservices-list/dataservices-list.component";
import { ViewsContentComponent } from "./dataservices-list/views-content.component";
import { DataservicesRoutingModule } from "./dataservices-routing.module";
import { DataservicesComponent } from "./dataservices.component";
import { SelectedNodeComponent } from "./selected-node/selected-node.component";
import { SelectedNodesListComponent } from "./selected-nodes-list/selected-nodes-list.component";
import { DataserviceService } from "./shared/dataservice.service";
import { MockDataserviceService } from "./shared/mock-dataservice.service";
import { MockVdbService } from "./shared/mock-vdb.service";
import { NotifierService } from "./shared/notifier.service";
import { VdbService } from "./shared/vdb.service";
import { SqlControlComponent } from "./sql-control/sql-control.component";
import { TestDataserviceComponent } from "./test-dataservice/test-dataservice.component";
import { ViewCanvasComponent } from "./virtualization/view-editor/view-canvas/view-canvas.component";
import { ViewEditorHeaderComponent } from "./virtualization/view-editor/view-editor-header/view-editor-header.component";
import { ViewEditorComponent } from "./virtualization/view-editor/view-editor.component";
import { ViewPreviewComponent } from "./virtualization/view-editor/editor-views/view-preview/view-preview.component";
import { EditorViewsComponent } from './virtualization/view-editor/editor-views/editor-views.component';
import { MessageLogComponent } from './virtualization/view-editor/editor-views/message-log/message-log.component';
import { CanvasService } from './virtualization/view-editor/view-canvas/canvas.service';
import { GraphVisualComponent, NodeVisualComponent, LinkVisualComponent } from './virtualization/view-editor/view-canvas/visuals';

import { environment } from "../../environments/environment";
import { ConfirmDialogComponent } from "../shared/confirm-dialog/confirm-dialog.component";
import { SharedModule } from "../shared/shared.module";
import { TreeModule } from "angular-tree-component";
import { CodemirrorModule } from "ng2-codemirror";
import {
  ActionModule,
  CardModule,
  EmptyStateModule,
  FilterModule,
  ListModule,
  NotificationModule,
  SortModule,
  TableModule,
  ToolbarModule,
  WizardModule } from "patternfly-ng";
import { OdataControlComponent } from "./odata-control/odata-control.component";
import { AccordionModule, BsDropdownModule, TabsModule, TooltipModule } from 'ngx-bootstrap';
import { ConnectionTreeSelectorComponent } from './virtualization/view-editor/connection-table-dialog/connection-tree-selector/connection-tree-selector.component';
import { ConnectionTableDialogComponent } from './virtualization/view-editor/connection-table-dialog/connection-table-dialog.component';
import { ProgressDialogComponent } from "../shared/progress-dialog/progress-dialog.component";
import { ViewPropertyEditorsComponent } from './virtualization/view-editor/view-property-editors/view-property-editors.component';
import { AddCompositionWizardComponent } from './virtualization/view-editor/add-composition-wizard/add-composition-wizard.component';
import { CreateViewDialogComponent } from './virtualization/view-editor/create-view-dialog/create-view-dialog.component';
import { CreateViewsDialogComponent } from './create-views-dialog/create-views-dialog.component';
import { SetDescriptionDialogComponent } from "./set-description-dialog/set-description-dialog.component";
import { PropertyEditorComponent } from './virtualization/view-editor/view-property-editors/property-editor/property-editor.component';
import { ProjectedColumnsEditorComponent } from './virtualization/view-editor/view-property-editors/projected-columns-editor/projected-columns-editor.component';
import { ViewsListComponent } from './virtualization/view-editor/views-list/views-list.component';

export const bsDropdownModule: ModuleWithProviders<any> = BsDropdownModule.forRoot();
export const accordionModule: ModuleWithProviders<any> = AccordionModule.forRoot();
export const tooltipModule: ModuleWithProviders<any> = TooltipModule.forRoot();
export const tabsModule: ModuleWithProviders<any> = TabsModule.forRoot();
export { LoggerService } from '../core/logger.service';
export { NotifierService } from './shared/notifier.service';
export { SelectionService } from '../core/selection.service';
export { CanvasService } from './virtualization/view-editor/view-canvas/canvas.service';

@NgModule({
  imports: [
    DataservicesRoutingModule,
    CommonModule,
    CoreModule,
    SharedModule,
    FormsModule,
    ReactiveFormsModule,
    RouterModule,
    CodemirrorModule,
    TreeModule,
    bsDropdownModule,
    accordionModule,
    tooltipModule,
    ActionModule,
    CardModule,
    EmptyStateModule,
    FilterModule,
    ListModule,
    NotificationModule,
    SortModule,
    TableModule,
    ToolbarModule,
    WizardModule,
    tabsModule
  ],
  declarations: [
    DataservicesDetailsComponent,
    ViewsContentComponent,
    DataservicesCardsComponent,
    DataservicesComponent,
    DataservicesListComponent,
    TestDataserviceComponent,
    SqlControlComponent,
    SelectedNodeComponent,
    OdataControlComponent,
    DataserviceCardComponent,
    SelectedNodesListComponent,
    ViewEditorComponent,
    ViewPreviewComponent,
    ViewEditorHeaderComponent,
    ViewCanvasComponent,
    MessageLogComponent,
    EditorViewsComponent,
    ConnectionTreeSelectorComponent,
    ConnectionTableDialogComponent,
    ViewPropertyEditorsComponent,
    GraphVisualComponent,
    NodeVisualComponent,
    LinkVisualComponent,
    AddCompositionWizardComponent,
    CreateViewDialogComponent,
    CreateViewsDialogComponent,
    SetDescriptionDialogComponent,
    PropertyEditorComponent,
    ProjectedColumnsEditorComponent,
    ViewsListComponent
  ],
  providers: [
    {
      provide: DataserviceService,
      useFactory: dataserviceServiceFactory,
      deps: [ Http, VdbService, AppSettingsService, NotifierService, LoggerService ],
      multi: false
    },
    {
      provide: VdbService,
      useFactory: vdbServiceFactory,
      deps: [ Http, AppSettingsService, NotifierService, LoggerService ],
      multi: false
    },
    LoggerService,
    NotifierService,
    SelectionService,
    CanvasService
  ],
  exports: [
    ViewEditorComponent
  ],
  entryComponents: [AddCompositionWizardComponent, ConfirmDialogComponent, ConnectionTableDialogComponent,
                    CreateViewDialogComponent, CreateViewsDialogComponent, ProgressDialogComponent, SetDescriptionDialogComponent]
})
export class DataservicesModule { }

/**
 * A factory that produces the appropriate instance of the service based on current environment settings.
 *
 * @param {Http} http the HTTP service
 * @param {VdbService} vdbService the VDB service
 * @param {AppSettingsService} appSettings the app settings service
 * @param {NotifierService} notifierService the notifier service
 * @param {LoggerService} logger the logger
 * @returns {DataserviceService} the requested service
 */
export function dataserviceServiceFactory( http: Http,
                                           vdbService: VdbService,
                                           appSettings: AppSettingsService,
                                           notifierService: NotifierService,
                                           logger: LoggerService ): DataserviceService {
  return environment.production || !environment.uiDevMode ? new DataserviceService( http,
                                                                                    vdbService,
                                                                                    appSettings,
                                                                                    notifierService,
                                                                                    logger )
                                                          : new MockDataserviceService( http,
                                                                                        vdbService,
                                                                                        appSettings,
                                                                                        notifierService,
                                                                                        logger );
}

/**
 * A factory that produces the appropriate instance of the service based on current environment settings.
 *
 * @param {Http} http the HTTP service
 * @param {AppSettingsService} appSettings the app settings service
 * @param {NotifierService} notifierService the notifier service
 * @param {LoggerService} logger the logger
 * @returns {VdbService} the requested service
 */
export function vdbServiceFactory( http: Http,
                                   appSettings: AppSettingsService,
                                   notifierService: NotifierService,
                                   logger: LoggerService ): VdbService {
  return environment.production || !environment.uiDevMode ? new VdbService( http,
                                                                            appSettings,
                                                                            notifierService,
                                                                            logger )
                                                           : new MockVdbService( http,
                                                                                 appSettings,
                                                                                 notifierService,
                                                                                 logger );
}
