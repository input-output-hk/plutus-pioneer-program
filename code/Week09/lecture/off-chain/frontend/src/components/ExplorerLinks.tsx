type Type = "address" | "policy" | "tx" | "datum" | "asset";

interface ExplorerLinkProps {
  type: Type;
  message: string;
  value: string;
}

interface ExplorerLinkPrimeProps {
  type: Type;
  message: string;
  value: string;
  link: string;
}

export const ExplorerLink = ({ type, message, value }: ExplorerLinkProps) => {
  return (
    <div className="font-quicksand h-16 w-full overflow-hidden">
      <p className="bg-zinc-800  text-base text-zinc-100 h-8 pt-[6px] pl-2">
        {message}
      </p>
      <a
        className="px-2 text-blue-500 hover:text-blue-800 underline"
        target="_blank"
        href={`https://preview.cexplorer.io/${type}/${value}`}
      >
        {value}
      </a>
    </div>
  );
};

export const ExplorerLinkPrime = ({
  type,
  message,
  value,
  link,
}: ExplorerLinkPrimeProps) => {
  return (
    <div className="font-quicksand h-16 w-full overflow-hidden">
      <p className="bg-zinc-800  text-base text-zinc-100 h-8 pt-[6px] pl-2">
        {message}
      </p>
      <a
        className="px-2 text-blue-500 hover:text-blue-800 underline"
        target="_blank"
        href={`https://preview.cexplorer.io/${type}/${link}`}
      >
        {value}
      </a>
    </div>
  );
};
