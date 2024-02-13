import React from "react";
import { toPng } from "html-to-image";
import { Button } from "./ui/button";

export function DownloadPlotButton({
  plotRef: ref,
  imageName: image_name
}: {
  plotRef: React.RefObject<HTMLDivElement>;
  imageName: string;
}) {
  const on_click = React.useCallback(() => {
    if (ref.current === null) {
      return;
    }
    toPng(ref.current, {
      pixelRatio: 2,
      cacheBust: true,
      backgroundColor: `white`
    })
      .then((dataUrl) => {
        const link = document.createElement("a");
        link.download = `${image_name}.png`;
        link.href = dataUrl;
        link.click();
      })
      .catch((err) => {
        console.log(err);
      });
  }, [ref]);

  return <Button onClick={on_click}>Download Image</Button>;
}
