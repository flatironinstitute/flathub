import "./globals.css";

export const metadata = {
  title: "Flatfront (next)",
};

export default function RootLayout({
  children,
}: {
  children: React.ReactNode;
}) {
  return (
    <html lang="en" className="dark">
      <body className="bg-slate-300 dark:bg-slate-800 dark:text-slate-50">
        {children}
      </body>
    </html>
  );
}
