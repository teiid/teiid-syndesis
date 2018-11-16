import { TestBed, inject } from '@angular/core/testing';
import { HttpModule } from "@angular/http";
import { LoggerService } from "../../../core/logger.service";
import { AppSettingsService } from "../../../core/app-settings.service";
import { MockAppSettingsService } from "../../../core/mock-app-settings.service";
import { VdbService } from "../../shared/vdb.service";
import { MockVdbService } from "../../shared/mock-vdb.service";
import { NotifierService } from "../../shared/notifier.service";
import { ViewEditorService } from "./view-editor.service";
import { DataserviceService } from "../../shared/dataservice.service";
import { MockDataserviceService } from "../../shared/mock-dataservice.service";
import { SelectionService } from "../../../core/selection.service";

describe('ViewEditorService', () => {
  beforeEach(() => {
    TestBed.configureTestingModule({
      imports: [
        HttpModule
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
    });
  });

  it('should be created', inject([ViewEditorService], ( service: ViewEditorService) => {
    expect(service).toBeTruthy();
  }));
});
