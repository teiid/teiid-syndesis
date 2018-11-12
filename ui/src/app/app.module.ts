import { BrowserModule } from '@angular/platform-browser';
import { NgModule } from '@angular/core';

import { AppComponent } from './app.component';
import { ConnectionsModule} from '../../projects/beetle-lib/src/connections/connections.module';
import { VirtualizationsModule } from '../../projects/beetle-lib/src/virtualizations/virtualizations.module';

@NgModule({
  declarations: [
    AppComponent
  ],
  imports: [
    BrowserModule,
    ConnectionsModule,
    VirtualizationsModule
  ],
  providers: [],
  bootstrap: [AppComponent]
})
export class AppModule { }
