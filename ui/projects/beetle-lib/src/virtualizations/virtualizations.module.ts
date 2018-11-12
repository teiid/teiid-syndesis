import { NgModule } from '@angular/core';
import { CommonModule } from '@angular/common';
import { VirtualizationsComponent } from './virtualizations.component';

@NgModule({
  imports: [
    CommonModule
  ],
  declarations: [VirtualizationsComponent],
  exports: [VirtualizationsComponent]
})
export class VirtualizationsModule { }
