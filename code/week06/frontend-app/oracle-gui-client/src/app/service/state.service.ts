import { Injectable } from '@angular/core';
import { of, ReplaySubject, Subject } from 'rxjs';
import { map, withLatestFrom } from 'rxjs/operators';
import { Wallet } from '../model/model';
import { IntegrationService } from './integration.service';

@Injectable({
  providedIn: 'root',
})
export class StateService {
  wallets$: Subject<Wallet[]> = new ReplaySubject(1);
  loading$: Subject<boolean> = new ReplaySubject(1);

  constructor(private integrationService: IntegrationService) {}

  public readWallets(): void {
    this.loading$.next(true);
    this.integrationService.readWallets().subscribe((wallets: Wallet[]) => {
      this.wallets$.next(wallets);
      this.loading$.next(false);
    });
  }


  public updateWallet(walletId: string): void {
    this.integrationService
      .readFunds(walletId)
      .pipe(withLatestFrom(this.wallets$),
      map(([wallet, wallets]: [Wallet, Wallet[]]) => {
        return [...wallets.filter((w: Wallet) => w.walletName != wallet.walletName), wallet]
      }))
      .subscribe((wallets: Wallet[]) => this.wallets$.next(wallets));
  }

  public use(walletId: string): void {
    this.loading$.next(true);
    this.integrationService.use(walletId).subscribe();
    this.readWallets();
  }
  public retrieve(walletId: string): void {
    this.loading$.next(true);
    this.integrationService.retreive(walletId).subscribe();
    this.readWallets();
  }

  public offerLovelaces(walletId: string, offer: number): void {
    this.loading$.next(true);
    this.integrationService.offerLovelace(walletId, {offeredLovelaces: offer}).subscribe();
    this.readWallets();
  }
}
