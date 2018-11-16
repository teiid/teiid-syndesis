import { ModuleWithProviders } from "@angular/core";
import { async, ComponentFixture, inject, TestBed } from "@angular/core/testing";
import { HttpModule } from "@angular/http";
import { RouterTestingModule } from "@angular/router/testing";
import { AboutDialogComponent } from "../about-dialog/about-dialog.component";
import { AboutService } from "../about-dialog/about.service";
import { AppSettingsService } from "../app-settings.service";
import { LoggerService } from "../logger.service";
import { MockAppSettingsService } from "../mock-app-settings.service";
import { VerticalNavComponent } from "./vertical-nav.component";
import { BsModalService, ModalModule } from "ngx-bootstrap";
import { ModalModule as PfngModalModule, NavigationModule as PfngNavigationModule } from "patternfly-ng";

export const moduleRoot: ModuleWithProviders<any> = ModalModule.forRoot();

describe("VerticalNavComponent", () => {
  let component: VerticalNavComponent;
  let fixture: ComponentFixture<VerticalNavComponent>;

  beforeEach(async(() => {
    TestBed.configureTestingModule({
      imports: [
        HttpModule,
        moduleRoot,
        RouterTestingModule,
        PfngModalModule,
        PfngNavigationModule
      ],
      declarations: [ VerticalNavComponent, AboutDialogComponent ],
      providers: [
        AboutService,
        { provide: AppSettingsService, useClass: MockAppSettingsService },
        BsModalService,
        LoggerService ]
    })
    .compileComponents().then(() => {
      // nothing to do
    });
  }));

  beforeEach(() => {
    fixture = TestBed.createComponent(VerticalNavComponent);
    component = fixture.componentInstance;
    fixture.detectChanges();
  });

  it("should be created", inject([ LoggerService ],
    (logger: LoggerService ) => {
    console.log("========== [VerticalNavComponent] should be created");
    expect(component).toBeTruthy();
  }));
});
