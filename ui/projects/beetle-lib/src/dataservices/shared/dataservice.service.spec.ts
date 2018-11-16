import { inject, TestBed } from "@angular/core/testing";
import { HttpModule } from "@angular/http";
import { AppSettingsService } from "../../core/app-settings.service";
import { LoggerService } from "../../core/logger.service";
import { MockAppSettingsService } from "../../core/mock-app-settings.service";
import { DataserviceService } from "./dataservice.service";
import { MockVdbService } from "./mock-vdb.service";
import { NotifierService } from "./notifier.service";
import { VdbService } from "./vdb.service";

describe("DataserviceService", () => {
  beforeEach(() => {
    TestBed.configureTestingModule({
      imports: [ HttpModule ],
      providers: [
        DataserviceService,
        { provide: AppSettingsService, useClass: MockAppSettingsService },
        LoggerService,
        NotifierService,
        { provide: VdbService, useClass: MockVdbService }
      ]
    });
  });

  it("should be created", inject([DataserviceService, AppSettingsService, LoggerService],
                                            ( service: DataserviceService ) => {
    console.log("========== [DataserviceService] should be created");
    expect(service).toBeTruthy();
  }));
});
