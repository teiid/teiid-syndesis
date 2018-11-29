import { async, ComponentFixture, TestBed } from "@angular/core/testing";
import { HttpModule } from "@angular/http";
import { About } from "./about.model";
import { AboutService } from "./about.service";
import { MockAboutService } from "./mock-about.service";
import { AppSettingsService } from "../app-settings.service";
import { LoggerService } from "../logger.service";
import { MockAppSettingsService } from "../mock-app-settings.service";
import { ModalModule } from "patternfly-ng";
import { AboutDialogComponent } from "./about-dialog.component";

describe("AboutDialogComponent", () => {
  let component: AboutDialogComponent;
  let fixture: ComponentFixture<AboutDialogComponent>;

  beforeEach(async(() => {
    TestBed.configureTestingModule({
      declarations: [ AboutDialogComponent ],
      imports: [ HttpModule, ModalModule ],
      providers: [
          AboutService,
          { provide: AppSettingsService, useClass: MockAppSettingsService },
      LoggerService ]
    })
      .compileComponents().then(() => {
      // nothing to do
    });
  }));

  beforeEach(() => {
    fixture = TestBed.createComponent(AboutDialogComponent);
    component = fixture.componentInstance;
    component.info = About.create( MockAboutService.json );
    fixture.detectChanges();
  });

  it("should be created", () => {
    console.log("========== [AboutDialogComponent] should be created");
    expect(component).toBeTruthy();
  });
});