import { BrowserModule } from "@angular/platform-browser";
import { NgModule } from "@angular/core";

import { AppComponent } from "./app.component";
import { ConnectionsModule } from "../../projects/beetle-lib/src/connections/connections.module";
import { DataservicesModule } from "../../projects/beetle-lib/src/dataservices/dataservices.module";
import { AppRoutingModule } from "./app-routing.module";
import { CoreModule } from "../../projects/beetle-lib/src/core/core.module";

@NgModule({
  declarations: [
    AppComponent
  ],
  imports: [
    BrowserModule,
    CoreModule,
    ConnectionsModule,
    DataservicesModule,
    AppRoutingModule
  ],
  providers: [],
  bootstrap: [AppComponent]
})
export class AppModule { }
