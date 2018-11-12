import { NgModule } from '@angular/core';
import { CommonModule } from '@angular/common';
import { ConnectionsComponent } from './connections.component';

@NgModule({
  imports: [
    CommonModule
  ],
  declarations: [ConnectionsComponent],
  exports: [ConnectionsComponent]
})
export class ConnectionsModule { }
