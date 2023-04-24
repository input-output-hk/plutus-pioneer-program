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
        <div>
            <b>{message}</b>
            <a
                className="text-blue-500 hover:text-blue-800 underline"
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
        <div>
            <b>{message}</b>
            <a
                className="text-blue-500 hover:text-blue-800 underline"
                target="_blank"
                href={`https://preview.cexplorer.io/${type}/${link}`}
            >
                {value}
            </a>
        </div>
    );
};
