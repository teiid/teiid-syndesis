import { async, ComponentFixture, TestBed } from '@angular/core/testing';
import { ModuleWithProviders } from "@angular/core";

import { SetDescriptionDialogComponent } from './set-description-dialog.component';
import { HttpModule } from "@angular/http";
import { BsModalRef, ModalModule } from "ngx-bootstrap";
import {
  ActionModule
} from "patternfly-ng";
import { FormsModule, ReactiveFormsModule } from "@angular/forms";
import { AppSettingsService } from "@core/app-settings.service";
import { LoggerService } from "@core/logger.service";

export const moduleRoot: ModuleWithProviders<any> = ModalModule.forRoot();

describe('SetDescriptionDialogComponent', () => {
  let component: SetDescriptionDialogComponent;
  let fixture: ComponentFixture<SetDescriptionDialogComponent>;

  beforeEach(async(() => {
    TestBed.configureTestingModule({
      imports: [
        HttpModule,
        FormsModule,
        ReactiveFormsModule,
        moduleRoot,
        ActionModule
      ],
      declarations: [ SetDescriptionDialogComponent ],
      providers: [ AppSettingsService, BsModalRef, LoggerService
      ]
    })
      .compileComponents();
  }));

  beforeEach(() => {
    fixture = TestBed.createComponent(SetDescriptionDialogComponent);
    component = fixture.componentInstance;
    fixture.detectChanges();
  });

  it('should be created', () => {
    expect(component).toBeTruthy();
  });
});
