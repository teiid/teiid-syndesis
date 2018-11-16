import { ModuleWithProviders } from "@angular/core";
import { async, ComponentFixture, TestBed } from '@angular/core/testing';
import { HttpModule } from "@angular/http";
import { LoggerService } from "../../../../core/logger.service";
import { MockAppSettingsService } from "../../../../core/mock-app-settings.service";
import { AppSettingsService } from "../../../../core/app-settings.service";
import { VdbService } from "../../../shared/vdb.service";
import { MockVdbService } from "../../../shared/mock-vdb.service";
import { DataserviceService } from "../../../shared/dataservice.service";
import { MockDataserviceService } from "../../../shared/mock-dataservice.service";
import { NotifierService } from "../../../shared/notifier.service";
import { ViewEditorService } from "../view-editor.service";
import { EditorViewsComponent } from "./editor-views.component";
import { MessageLogComponent } from "./message-log/message-log.component";
import { ViewPreviewComponent } from "./view-preview/view-preview.component";
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
  WizardModule } from "patternfly-ng";
import { SelectionService } from "../../../../core/selection.service";

export const tabsModule: ModuleWithProviders<any> = TabsModule.forRoot();

describe('EditorViewsComponent', () => {
  let component: EditorViewsComponent;
  let fixture: ComponentFixture<EditorViewsComponent>;

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
        tabsModule
      ],
      declarations: [
        EditorViewsComponent,
        MessageLogComponent,
        ViewPreviewComponent
      ],
      providers: [
        { provide: AppSettingsService, useClass: MockAppSettingsService },
        { provide: DataserviceService, useClass: MockDataserviceService },
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
    fixture = TestBed.createComponent(EditorViewsComponent);
    component = fixture.componentInstance;
    fixture.detectChanges();
  });

  it('should be created', () => {
    expect(component).toBeTruthy();
  });
});
