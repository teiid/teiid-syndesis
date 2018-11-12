import { BrowserModule } from '@angular/platform-browser';
import { NgModule } from '@angular/core';

import { AppComponent } from './app.component';
import { BeetleLibModule } from '../../projects/beetle-lib/src/lib/beetle-lib.module';

@NgModule({
  declarations: [
    AppComponent
  ],
  imports: [
    BrowserModule,
    BeetleLibModule
  ],
  providers: [],
  bootstrap: [AppComponent]
})
export class AppModule { }
