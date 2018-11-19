import { Component, ViewEncapsulation } from "@angular/core";

@Component({
  selector: 'btl-style-lib',
  template: '<ng-content></ng-content>',
  styleUrls: ['./style-lib.component.css', '../../styles.css'],
  encapsulation: ViewEncapsulation.None
})
export class StyleLibComponent {

  constructor() {
    // Nothing to do
  }

}
