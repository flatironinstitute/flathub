import * as React from 'react';
import { Routes, Route } from 'react-router-dom';
import { Catalog } from './types/Catalog';
const Home = React.lazy(() => import('./components/Home'));
const Catalogs = React.lazy(() => import('./components/Catalogs'));
const Collections = React.lazy(() => import('./components/Collections'));
const About = React.lazy(() => import('./components/About'));
const Loading = () => <p>Loading ...</p>;

const fakes: Catalog[] = [
  {
    name: 'Calvin Hawkins',
    city: 'calvin.hawkins@example.com',
    logo: 'https://images.unsplash.com/photo-1491528323818-fdd1faba62cc?ixlib=rb-1.2.1&ixid=eyJhcHBfaWQiOjEyMDd9&auto=format&fit=facearea&facepad=2&w=256&h=256&q=80',
  },
  {
    name: 'Kristen Ramos',
    city: 'kristen.ramos@example.com',
    logo: 'https://images.unsplash.com/photo-1550525811-e5869dd03032?ixlib=rb-1.2.1&ixid=eyJhcHBfaWQiOjEyMDd9&auto=format&fit=facearea&facepad=2&w=256&h=256&q=80',
  },
  {
    name: 'Ted Fox',
    city: 'ted.fox@example.com',
    logo: 'https://images.unsplash.com/photo-1500648767791-00dcc994a43e?ixlib=rb-1.2.1&ixid=eyJhcHBfaWQiOjEyMDd9&auto=format&fit=facearea&facepad=2&w=256&h=256&q=80',
  },
];

const Main = () => {
  return (
    <React.Suspense fallback={<Loading />}>
      <Routes>
        <Route path="/" element={<Home />} />
        <Route path="/catalogs" element={<Catalogs catalogs={fakes} />} />
        <Route path="/collections" element={<Collections />} />
        <Route path="/about" element={<About />} />
      </Routes>
    </React.Suspense>
  );
};
export default Main;
