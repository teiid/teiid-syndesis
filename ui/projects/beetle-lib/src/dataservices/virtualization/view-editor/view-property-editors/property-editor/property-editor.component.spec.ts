import { async, ComponentFixture, TestBed } from '@angular/core/testing';

import { PropertyEditorComponent } from './property-editor.component';
import { SelectionService } from "../../../../../core/selection.service";
import { ViewEditorService } from "../../view-editor.service";
import { LoggerService } from "../../../../../core/logger.service";
import { DataserviceService } from "../../../../shared/dataservice.service";
import { MockDataserviceService } from "../../../../shared/mock-dataservice.service";
import { HttpModule } from "@angular/http";
import { VdbService } from "../../../../shared/vdb.service";
import { MockVdbService } from "../../../../shared/mock-vdb.service";
import { AppSettingsService } from "../../../../../core/app-settings.service";
import { MockAppSettingsService } from "../../../../../core/mock-app-settings.service";
import { NotifierService } from "../../../../shared/notifier.service";

describe('PropertyEditorComponent', () => {
  let component: PropertyEditorComponent;
  let fixture: ComponentFixture<PropertyEditorComponent>;

  beforeEach(async(() => {
    TestBed.configureTestingModule({
      imports: [
        HttpModule,
      ],
      declarations: [ PropertyEditorComponent ],
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
    .compileComponents();
  }));

  beforeEach(() => {
    fixture = TestBed.createComponent(PropertyEditorComponent);
    component = fixture.componentInstance;
    fixture.detectChanges();
  });

  it('should be created', () => {
    expect(component).toBeTruthy();
  });
});
