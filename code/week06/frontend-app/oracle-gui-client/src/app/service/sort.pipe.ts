import { Pipe, PipeTransform } from '@angular/core';
import { Wallet } from '../model/model';

@Pipe({
  name: 'sort'
})
export class SortPipe implements PipeTransform {

  transform(wallets: Wallet[] | null): Wallet[] {
  if(!wallets){
    return [];
  }
  return wallets.slice(0).sort((a: Wallet, b: Wallet) => {
    
    if(isNaN(+a.walletName)){
      return -1;
    } else if(isNaN(+b.walletName)){
      return 1;
    }
    return +a.walletName - +b.walletName
  });

}
}
