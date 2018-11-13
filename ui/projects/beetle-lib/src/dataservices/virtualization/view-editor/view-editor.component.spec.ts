import { ModuleWithProviders } from "@angular/core";
import { async, ComponentFixture, TestBed } from '@angular/core/testing';
import { FormsModule } from "@angular/forms";
import { RouterTestingModule } from "@angular/router/testing";
import { ConnectionService } from "../../../connections/shared/connection.service";
import { MockConnectionService } from "../../../connections/shared/mock-connection.service";
import { CoreModule } from "../../../core/core.module";
import { AppSettingsService } from "../../../core/app-settings.service";
import { MockAppSettingsService } from "../../../core/mock-app-settings.service";
import { SelectionService } from "../../../core/selection.service";
import { MockVdbService } from "../../shared/mock-vdb.service";
import { NotifierService } from "../../shared/notifier.service";
import { VdbService } from "../../shared/vdb.service";
import { ViewEditorComponent } from '../../virtualization/view-editor/view-editor.component';
import { ViewCanvasComponent } from "../../virtualization/view-editor/view-canvas/view-canvas.component";
import { ConnectionTableDialogComponent } from "../../virtualization/view-editor/connection-table-dialog/connection-table-dialog.component";
import { ConnectionTreeSelectorComponent } from "../../virtualization/view-editor/connection-table-dialog/connection-tree-selector/connection-tree-selector.component";
import { EditorViewsComponent } from "../../virtualization/view-editor/editor-views/editor-views.component";
import { MessageLogComponent } from "../../virtualization/view-editor/editor-views/message-log/message-log.component";
import { ViewPreviewComponent } from "../../virtualization/view-editor/editor-views/view-preview/view-preview.component";
import { ViewEditorHeaderComponent } from "../../virtualization/view-editor/view-editor-header/view-editor-header.component";
import { ViewPropertyEditorsComponent } from "../../virtualization/view-editor/view-property-editors/view-property-editors.component";
import { TreeModule } from "angular-tree-component";
import { TabsModule } from "ngx-bootstrap";
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
import {
  GraphVisualComponent,
  LinkVisualComponent,
  NodeVisualComponent
} from "../../virtualization/view-editor/view-canvas/visuals";
import { CanvasService } from "../../virtualization/view-editor/view-canvas/canvas.service";

export const tabsModule: ModuleWithProviders<any> = TabsModule.forRoot();

describe('ViewEditorComponent', () => {
  let component: ViewEditorComponent;
  let fixture: ComponentFixture<ViewEditorComponent>;

  beforeEach(async(() => {
    TestBed.configureTestingModule({
      imports: [
        CoreModule,
        FormsModule,
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
        RouterTestingModule,
        tabsModule,
        TreeModule
      ],
      declarations: [
        ConnectionTableDialogComponent,
        ConnectionTreeSelectorComponent,
        EditorViewsComponent,
        GraphVisualComponent,
        LinkVisualComponent,
        NodeVisualComponent,
        MessageLogComponent,
        ViewCanvasComponent,
        ViewEditorComponent,
        ViewEditorHeaderComponent,
        ViewPreviewComponent,
        ViewPropertyEditorsComponent
      ],
      providers: [
        {provide: AppSettingsService, useClass: MockAppSettingsService},
        CanvasService,
        {provide: ConnectionService, useClass: MockConnectionService},
        NotifierService,
        SelectionService,
        {provide: VdbService, useClass: MockVdbService}
      ]
    })
      .compileComponents().then(() => {
      // nothing to do
    });
  }));

  beforeEach(() => {
    fixture = TestBed.createComponent(ViewEditorComponent);
    component = fixture.componentInstance;
    fixture.detectChanges();
  });

  // it('should be created', () => {
  //   expect(component).toBeTruthy();
  // });
});
