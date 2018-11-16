import { Component, OnInit } from "@angular/core";
import { BsModalRef } from "ngx-bootstrap";
import { Subject } from "rxjs/Subject";

@Component({
  selector: "btl-confirm-dialog",
  templateUrl: "./confirm-dialog.component.html",
  styleUrls: ["./confirm-dialog.component.css"]
})
/**
 * Confirmation Dialog.  Invoke this from another component as follows:
 *
 *     this.modalRef = this.modalService.show(ConfirmDialogComponent, {initialState});
 *     this.modalRef.content.confirmAction.take(1).subscribe((value) => {
 *       this.doSomethingOnConfirm();
 *     });
 *
 *     The expected initial state is as follows:
 *     const initialState = {
 *       title: "The dialog title",
 *       bodyContent: "The dialog message",
 *       cancelButtonText: "Text for cancel button",
 *       confirmButtonText: "Text for confirm button"
 *     };
 */
export class ConfirmDialogComponent implements OnInit {

  public confirmAction: Subject<boolean>;
  public cancelAction: Subject<boolean>;

  public title = "Title";
  public bodyContent = "Confirmation Message";
  public cancelButtonText = "Cancel";
  public confirmButtonText = "Confirm";
  public bsModalRef: BsModalRef;

  constructor(bsModalRef: BsModalRef) {
    this.bsModalRef = bsModalRef;
  }

  public ngOnInit(): void {
    this.confirmAction = new Subject();
    this.cancelAction = new Subject();
  }

  public onConfirmSelected(): void {
    this.confirmAction.next(true);
    this.bsModalRef.hide();
  }

  public onCancelSelected(): void {
    this.cancelAction.next(true);
    this.bsModalRef.hide();
  }

}
