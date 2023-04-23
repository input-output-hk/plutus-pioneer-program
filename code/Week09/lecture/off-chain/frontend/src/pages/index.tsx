import Link from "next/link";

export default function Home() {
    return (
        <main className="flex min-h-screen flex-col items-center justify-center pt-4 px-10">
            <h1 className=" text-xl text-orange-600 uppercase pb-5">
                <b>Choose your side</b>
            </h1>
            <Link href="owner">Go to Owner UI</Link>
            <Link href="user">Go to User UI</Link>
        </main>
    );
}
