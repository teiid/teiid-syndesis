import { ModuleWithProviders } from "@angular/core";
import { async, ComponentFixture, TestBed } from '@angular/core/testing';
import { HttpModule } from "@angular/http";
import { LoggerService } from "../../../../core/logger.service";
import { ModalModule } from "ngx-bootstrap";
import { MockAppSettingsService } from "../../../../core/mock-app-settings.service";
import { AppSettingsService } from "../../../../core/app-settings.service";
import { ViewCanvasComponent } from "./view-canvas.component";
import { ViewEditorService } from "../view-editor.service";
import {
  ActionModule,
  CardModule,
  EmptyStateModule,
  FilterModule,
  ListModule,
  NotificationModule,
  SortModule,
  TableModule,
  WizardModule } from "patternfly-ng";
import { DataserviceService } from "../../../shared/dataservice.service";
import { MockDataserviceService } from "../../../shared/mock-dataservice.service";
import { VdbService } from "../../../shared/vdb.service";
import { MockVdbService } from "../../../shared/mock-vdb.service";
import { NotifierService } from "../../../shared/notifier.service";
import { ViewPropertyEditorsComponent } from "../view-property-editors/view-property-editors.component";
import { TabsModule } from "ngx-bootstrap";
import { GraphVisualComponent, LinkVisualComponent, NodeVisualComponent } from "./visuals";
import { CanvasService } from "./canvas.service";
import { SelectionService } from "../../../../core/selection.service";
import { PropertyEditorComponent } from "../view-property-editors/property-editor/property-editor.component";
import { ProjectedColumnsEditorComponent } from "../view-property-editors/projected-columns-editor/projected-columns-editor.component";
import { ViewsListComponent } from "../views-list/views-list.component";
import { BsModalService } from "ngx-bootstrap";
import { Dataservice } from "../../../shared/dataservice.model";

export const tabsModule: ModuleWithProviders<any> = TabsModule.forRoot();
export const modalModule: ModuleWithProviders<any> = ModalModule.forRoot();

describe('ViewCanvasComponent', () => {
  let component: ViewCanvasComponent;
  let fixture: ComponentFixture<ViewCanvasComponent>;

  beforeEach(async(() => {
    TestBed.configureTestingModule({
      imports: [
        ActionModule,
        CardModule,
        EmptyStateModule,
        FilterModule,
        ListModule,
        NotificationModule,
        SortModule,
        TableModule,
        WizardModule,
        HttpModule,
        modalModule,
        tabsModule
      ],
      declarations: [
        GraphVisualComponent,
        LinkVisualComponent,
        NodeVisualComponent,
        ProjectedColumnsEditorComponent,
        PropertyEditorComponent,
        ViewCanvasComponent,
        ViewPropertyEditorsComponent,
        ViewsListComponent
      ],
      providers: [
        BsModalService,
        { provide: AppSettingsService, useClass: MockAppSettingsService },
        { provide: DataserviceService, useClass: MockDataserviceService },
        CanvasService,
        LoggerService,
        NotifierService,
        SelectionService,
        { provide: VdbService, useClass: MockVdbService },
        ViewEditorService
      ]
    })
    .compileComponents().then(() => {
      // nothing to do
    });
  }));

  beforeEach(() => {
    fixture = TestBed.createComponent(ViewCanvasComponent);
    component = fixture.componentInstance;

    const selService = TestBed.get( SelectionService );
    const ds: Dataservice = new Dataservice();
    ds.setId("testDs");
    ds.setServiceVdbName("testDsVdb");
    // noinspection JSUnusedAssignment
    selService.setSelectedVirtualization( ds );

    fixture.detectChanges();
  });

  it('should be created', () => {
    expect(component).toBeTruthy();
  });
});
