import {
  ChangeDetectionStrategy,
  Component,
  Input,
  OnChanges,
  OnInit,
  SimpleChange,
  SimpleChanges,
} from '@angular/core';
import { Wallet } from '../model/model';
import { StateService } from '../service/state.service';

@Component({
  selector: 'app-wallet',
  templateUrl: './wallet.component.html',
  styleUrls: ['./wallet.component.css'],
  changeDetection: ChangeDetectionStrategy.OnPush,
})
export class WalletComponent {
  @Input('wallet') wallet: Wallet = { walletName: '', walletUuid: '' };
  constructor(private stateService: StateService) {}


  retrieve(): void {
    this.stateService.retrieve(this.wallet.walletName);
  
  }

  use(): void {
    this.stateService.use(this.wallet.walletName);

  }

  offer(value: number): void{
    this.stateService.offerLovelaces(this.wallet.walletName, value);
    
  }
}
