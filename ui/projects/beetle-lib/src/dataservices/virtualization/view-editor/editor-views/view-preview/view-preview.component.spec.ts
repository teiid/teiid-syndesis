import { HttpModule } from "@angular/http";
import { async, ComponentFixture, TestBed } from "@angular/core/testing";
import { RouterTestingModule } from "@angular/router/testing";
import { LoggerService } from "../../../../../core/logger.service";
import { MockAppSettingsService } from "../../../../../core/mock-app-settings.service";
import { AppSettingsService } from "../../../../../core/app-settings.service";
import { ViewEditorService } from "../../view-editor.service";
import { ViewPreviewComponent } from "./view-preview.component";
import {
  ActionModule,
  CardModule,
  EmptyStateModule,
  FilterModule,
  ListModule,
  SortModule,
  TableModule,
  WizardModule } from "patternfly-ng";
import { DataserviceService } from "../../../../shared/dataservice.service";
import { MockDataserviceService } from "../../../../shared/mock-dataservice.service";
import { VdbService } from "../../../../shared/vdb.service";
import { MockVdbService } from "../../../../shared/mock-vdb.service";
import { NotifierService } from "../../../../shared/notifier.service";
import { SelectionService } from "../../../../../core/selection.service";

describe("ViewPreviewComponent", () => {
  let component: ViewPreviewComponent;
  let fixture: ComponentFixture<ViewPreviewComponent>;

  beforeEach(async(() => {
    TestBed.configureTestingModule({
      imports: [
        ActionModule,
        CardModule,
        EmptyStateModule,
        FilterModule,
        ListModule,
        SortModule,
        TableModule,
        WizardModule,
        HttpModule,
        RouterTestingModule
      ],
      declarations: [ ViewPreviewComponent ],
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
    fixture = TestBed.createComponent(ViewPreviewComponent);
    component = fixture.componentInstance;
    fixture.detectChanges();
  });

  it("should be created", () => {
    expect(component).toBeTruthy();
  });
});
