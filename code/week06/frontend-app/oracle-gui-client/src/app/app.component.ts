import { ChangeDetectionStrategy, Component, OnInit } from '@angular/core';
import { Observable, of, ReplaySubject, Subject } from 'rxjs';
import { Wallet } from './model/model';
import { IntegrationService } from './service/integration.service';
import { StateService } from './service/state.service';

@Component({
  selector: 'app-root',
  templateUrl: './app.component.html',
  styleUrls: ['./app.component.css'],
  changeDetection: ChangeDetectionStrategy.OnPush
})
export class AppComponent implements OnInit {
  wallets$: Observable<Wallet[]> = of();
  loading$: Observable<boolean> = of();

  constructor(private stateService: StateService){

  }

  ngOnInit() : void {
   this.stateService.readWallets();
   this.wallets$ = this.stateService.wallets$;
   this.loading$ = this.stateService.loading$;

  }

  trackByWalletName(index: number, wallet: Wallet): string {
    return wallet.walletName;
  }
  
}
