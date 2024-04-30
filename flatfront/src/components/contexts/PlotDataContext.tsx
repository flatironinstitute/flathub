import React from "react";
import { useImmer, type Updater } from "use-immer";
import { usePlotID } from "./PlotContext";

/**
 * This is a way to store global plot data *outside* of the global App State.
 */

type PlotData = {
  [key: string]: any;
};

const PlotDataContext = React.createContext<PlotData | undefined>(undefined);
const SetPlotDataContext = React.createContext<Updater<PlotData> | undefined>(
  undefined
);

export function PlotDataProvider({ children }) {
  const [plot_data, set_plot_data] = useImmer<PlotData>({});
  return (
    <PlotDataContext.Provider value={plot_data}>
      <SetPlotDataContext.Provider value={set_plot_data}>
        {children}
      </SetPlotDataContext.Provider>
    </PlotDataContext.Provider>
  );
}

export function usePlotData() {
  const value = React.useContext(PlotDataContext);
  if (value === undefined) {
    throw new Error(`usePlotData must be used within a Provider`);
  }
  return value;
}

export function useSetPlotData(data: any) {
  const set_plot_data = React.useContext(SetPlotDataContext);
  if (set_plot_data === undefined) {
    throw new Error(`useSetPlotData must be used within a Provider`);
  }
  const plot_id = usePlotID();
  React.useEffect(() => {
    set_plot_data((draft) => {
      draft[plot_id] = data;
    });
  }, [data]);
}
