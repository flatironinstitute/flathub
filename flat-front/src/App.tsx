import React, { useState } from "react";
import logo from './logo.svg';
import { Catalogs } from './features/catalogs/Catalogs';
import Header from './components/Header/Header';
import Table from './components/Table/Table';
import TableShell from './components/TableShell/TableShell';
import './App.css';

function App() {
  return (
    <div>
      <Header />
      <TableShell />
      <Table />
    </div>
  );
}

export default App;
