using System;
using Microsoft.AspNetCore.Components;
using Microsoft.AspNetCore.Components.Rendering;
using Microsoft.AspNetCore.Components.Web;

namespace TestBlazor
{
  public class FormletComponent : ComponentBase
  {
    protected override void BuildRenderTree(RenderTreeBuilder builder)
    {
      builder.OpenElement(1, "p");
      builder.AddContent(2, $"Hello - {this.currentCount}!");
      builder.CloseElement();
      builder.OpenElement(3, "button");
      builder.AddAttribute(4, "class", "btn");
      builder.AddAttribute(4, "class", "btn-primary");
      builder.AddAttribute<MouseEventArgs>(5, "onclick", EventCallback.Factory.Create<MouseEventArgs>(this, new Action(this.IncrementCount)));
      builder.AddContent(6, "Click me too!");
      builder.CloseElement();

    }

    void IncrementCount()
    {
      this.currentCount++;
    }

    int currentCount = 0;
  }
}
